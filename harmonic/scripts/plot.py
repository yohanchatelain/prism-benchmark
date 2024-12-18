#!/usr/bin/env python3

import glob
import os
import pandas as pd
import numpy as np
import re
import argparse
import plotly.express as px

import plotly.io as pio

pio.kaleido.scope.mathjax = None


def extract_hex(line):
    return float.fromhex(line.split("(")[1].split(")")[0])


def extract_values_default(file):
    with open(file, "r") as f:
        values = map(extract_hex, f.read().splitlines())
    return np.fromiter(values, dtype=np.float64)


def extract_cadna_line(line):
    # RESULT:  +5.18738175e+00 -- +5.18737698e+00 -- +5.18738509e+00
    return re.findall(r"\+\d+\.\d+e[+-]\d+", line)


def extract_values_cadna(file):
    with open(file, "r") as f:
        values = map(extract_cadna_line, f.read().splitlines())
        # flatten list of lists
        values = [item for sublist in values for item in sublist]
    return np.fromiter(values, dtype=np.float64)


def extract_values(file, tool):
    if "cadna" in tool:
        return extract_values_cadna(file)
    else:
        return extract_values_default(file)


def load_data(directory):
    files = glob.glob(os.path.join(directory, "*.txt"))
    modes = {"perf": [], "dbg": []}
    df = pd.DataFrame(columns=["tool", "mode", "iterations"])
    for file in files:
        tool, mode, iterations = os.path.splitext(os.path.basename(file))[0].split("-")
        values = extract_values(file, tool)
        mean = np.mean(values)
        std = np.std(values)
        _df = pd.DataFrame(
            {
                "tool": [tool],
                "mode": [mode],
                "iterations": [int(iterations)],
                "mean": [mean],
                "std": [std],
            }
        )
        df = pd.concat([df, _df])
    return df


def plot(df, std):
    df.sort_values(by=["iterations"], inplace=True)
    df_perf = df[df["mode"] == "perf"]
    if std:
        fig = px.scatter(
            df_perf,
            x="iterations",
            y="std",
            color="tool",
            #           facet_col="mode",
            log_x=True,
            log_y=True,
        )
    else:
        fig = px.scatter(
            df_perf,
            x="iterations",
            y="mean",
            color="tool",
            #            facet_col="mode",
            # markers=True,
            log_x=True,
            # error_y="std",
        )

    marker_map = {
        "baseline_double": "star",
        "baseline_float": "hexagram",
        "verrou_cestac": "x",
        "cadna": "x",
        "verrou_sr": "triangle-up",
        "prism_sr": "triangle-up",
        "prism_ud": "diamond",
        "sr": "triangle-up",
        "verificarlo": "triangle-up",
    }

    for tool in df["tool"].unique():
        marker = marker_map.get(tool, "diamond")
        fig.update_traces(marker_symbol=marker, selector=dict(name=tool))

    legend_map = {
        "prism_sr": "PRISM SR",
        "baseline_double": "IEEE binary64",
        "cadna": "CESTAC",
        "verificarlo": "MCA RR",
        "baseline_float": "IEEE binary32",
        "verrou_sr": "Verrou SR",
        "sr": "SR",
        "prism_ud": "PRISM UD",
        "verrou_cestac": "Verrou CESTAC",
    }

    legendgroup_map = {
        "prism_sr": "PRISM",
        "baseline_double": "IEEE",
        "cadna": "CADNA",
        "verificarlo": "MCA",
        "baseline_float": "IEEE",
        "verrou_sr": "VERROU",
        "sr": "SR",
        "prism_ud": "PRISM",
        "verrou_cestac": "VERROU",
    }

    fig.for_each_trace(lambda t: t.update(legendgroup=(legendgroup_map[t.name])))

    for tool in df["tool"].unique():
        new_name = legend_map.get(tool, tool)
        fig.for_each_trace(lambda t: t.update(name=new_name) if t.name == tool else ())

    color_map = {
        "PRISM": px.colors.qualitative.Plotly[0],  # blue
        "VERROU": px.colors.qualitative.Plotly[9],  # red
        "CADNA": px.colors.qualitative.Plotly[2],  # green
        "IEEE": px.colors.qualitative.Plotly[3],  # yellow
        "IEEE": px.colors.qualitative.Plotly[4],  # purple
        "SR": px.colors.qualitative.Plotly[5],  # purple
        "MCA": px.colors.qualitative.Plotly[1],  #
    }

    for trace in fig.data:
        trace.update(marker=dict(color=color_map[trace.legendgroup]))
        trace.update(line=dict(color=color_map[trace.legendgroup]))

    fig.update_traces(marker=dict(size=6))
    fig.update_traces(marker=dict(opacity=0.85))

    ytitle = "Standard Deviation" if std else "Average"
    xtitle = "Iteration"
    if std:
        fig.update_yaxes(type="log", exponentformat="power")
    fig.update_yaxes(title=ytitle)
    fig.update_xaxes(type="log", exponentformat="power", title=xtitle)

    if std:
        # hide FLOAT and DOUBLE
        fig.update_traces(visible=True, selector=dict(name="IEEE binary32"))
        fig.update_traces(visible=True, selector=dict(name="IEEE binary64"))
    else:
        fig.update_xaxes(range=[6, 7.1])
        fig.update_yaxes(range=[14, 22])

    fig.update_layout(legend_title_text="Method")
    fig.update_layout(font=dict(size=14))

    fig.update_layout(
        legend=dict(orientation="h", yanchor="bottom", y=1.02, xanchor="center", x=0.5)
    )

    fig.update_layout(
        legend=dict(
            orientation="h",
            yanchor="bottom",
            y=1.02,
            xanchor="center",
            x=0.5,
            itemwidth=30,  # Increase this value to increase space between labels
            itemsizing="constant",
            font=dict(size=12),  # Reduce font size of legend
        )
    )

    if not std:
        # do not show legend for mean
        fig.update_layout(showlegend=False)

    fig.update_layout(margin=dict(l=20, r=20, t=20, b=10))
    fig.update_layout(
        xaxis=dict(
            title=dict(standoff=0)  # Adjust this value to move the x-axis title up
        )
    )
    return fig


def parse_args():
    parser = argparse.ArgumentParser(description="Plot data from a directory")
    parser.add_argument("directory", type=str, help="Directory containing the data")
    parser.add_argument("--std", action="store_true", help="Plot standard deviation")
    parser.add_argument("--show", action="store_true")
    args = parser.parse_args()
    return args


def main():
    args = parse_args()
    df = load_data(args.directory)
    print(df)
    fig = plot(df, args.std)
    if args.show:
        fig.show()
    namefig = "harmonic-" + ("std" if args.std else "mean") + ".pdf"
    fig.write_image(namefig)


if "__main__" == __name__:
    main()
