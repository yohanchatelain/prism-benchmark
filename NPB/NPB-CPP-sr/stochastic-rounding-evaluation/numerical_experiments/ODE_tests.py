import numpy as np

import plotly as plt
import plotly.graph_objects as go
from chop import chop


def decay_ODE(accurate, x, y, options):
    if accurate:
        return -y / 20
    else:
        return -chop(y / 20, options)


def Euler(accurate, h, x, y, SR, options, precision):
    if accurate:
        return y + h * decay_ODE(1, x, y, options)
    elif SR:
        decay = decay_ODE(0, x, y, options)
        mulfma = srMultFMA(h, decay, np.random.rand(), options, precision)
        return srSum(y, mulfma, np.random.rand(), options, precision)
    else:
        return chop(
            y + chop(h * chop(decay_ODE(0, x, y, options), options), options), options
        )


def chop(value, options=None):
    # Implement the chop function based on the options
    # This is a placeholder implementation
    return value


def srMultFMA(a, b, R, options, precision):
    options["round"] = 4
    sigma, error = TwoMultFMA(a, b, options)
    Es = np.floor(np.log2(abs(sigma)))
    P = np.sign(error) * R * 2 ** (Es - precision)
    f = chop(chop(error + P) + sigma)
    options["round"] = 1
    return f


def TwoMultFMA(a, b, options):
    sigma = chop(a * b, options)
    error = chop(a * b - sigma, options)
    return sigma, error


def srSum(a, b, R, options, precision):
    options["round"] = 1
    sum_, error = TwoSum(a, b, options)
    options["round"] = 4
    Es = np.floor(np.log2(abs(chop(a + b, options))))
    P = np.sign(error) * R * 2 ** (Es - precision)
    if error >= 0:
        options["round"] = 3
    else:
        options["round"] = 2
    f = chop(chop(error + P) + sum_)
    options["round"] = 1
    return f


def TwoSum(a, b, options):
    sum_ = chop(a + b, options)
    a_trunc = chop(sum_ - b)
    b_trunc = chop(sum_ - a_trunc)
    a_error = chop(a - a_trunc)
    b_error = chop(b - b_trunc)
    error = chop(a_error + b_error)
    return sum_, error


def main():

    # Set up some ODE conditions
    a = 0
    b = 0.015625
    y0 = 1.0

    # Exact solution to the exponential decay ODE
    yexact = np.exp(-0.015625 / 20) * y0

    # Range of timesteps
    nrange = np.logspace(1, 6, 16, base=10, dtype=int)
    m = len(nrange)

    # Standard double precision
    efp = np.zeros((m, 7))
    for j in range(m):
        n = nrange[j]
        x_dp = a
        h_dp = (b - a) / n
        y_dp = y0

        for i in range(n):
            y_dp = Euler(1, h_dp, x_dp, y_dp, 0, None, 52)
            x_dp += h_dp

        efp[j, 0] = abs(y_dp - yexact)

    # Chop options
    options = {"round": 1, "format": "b", "subnormal": 1}
    precision = 7

    # All chop formats
    for k in range(6):
        if k == 0:
            options["format"] = "b"
            precision = 7
            sr = 0
        elif k == 1:
            options["format"] = "b"
            precision = 7
            sr = 1
        elif k == 2:
            options["format"] = "h"
            precision = 10
            sr = 0
        elif k == 3:
            options["format"] = "h"
            precision = 10
            sr = 1
        elif k == 4:
            options["format"] = "s"
            precision = 23
            sr = 0
        elif k == 5:
            options["format"] = "s"
            precision = 23
            sr = 1

        print(
            f'k = {k + 1}, prec = {options["format"]}, subnormal = {options["subnormal"]}'
        )

        for j in range(m):
            n = nrange[j]
            x_fp = a
            h_fp = (b - a) / n
            y_fp = y0

            for i in range(n):
                y_fp = Euler(0, h_fp, x_fp, y_fp, sr, options, precision)

            efp[j, k + 1] = abs(y_fp - yexact)

    fig = go.Figure()
    fig.add_trace(
        go.Scatter(
            x=nrange,
            y=efp[:, 0],
            mode="lines+markers",
            name="fp64",
            marker=dict(symbol="diamond"),
        )
    )
    fig.add_trace(
        go.Scatter(
            x=nrange,
            y=efp[:, 1],
            mode="lines+markers",
            name="bfloat16 RN",
            marker=dict(symbol="x"),
        )
    )
    fig.add_trace(
        go.Scatter(
            x=nrange,
            y=efp[:, 2],
            mode="lines+markers",
            name="bfloat16 SR",
            marker=dict(symbol="star"),
        )
    )
    fig.add_trace(
        go.Scatter(
            x=nrange,
            y=efp[:, 3],
            mode="lines+markers",
            name="fp16 RN",
            marker=dict(symbol="circle"),
        )
    )
    fig.add_trace(
        go.Scatter(
            x=nrange,
            y=efp[:, 4],
            mode="lines+markers",
            name="fp16 SR",
            marker=dict(symbol="square"),
        )
    )
    fig.add_trace(
        go.Scatter(
            x=nrange,
            y=efp[:, 5],
            mode="lines+markers",
            name="fp32 RN",
            marker=dict(symbol="circle"),
        )
    )
    fig.add_trace(go.Scatter(x=nrange, y=efp[:, 6], mode="lines", name="fp32 SR"))
    fig.update_yaxes(exponentformat="e")
    fig.update_xaxes(exponentformat="e")
    fig.update_layout(
        xaxis_type="log",
        yaxis_type="log",
        xaxis_title="$n$",
        yaxis_title="Error",
        title="Error vs Number of Steps",
        legend_title="Precision",
        font=dict(size=14),
        xaxis=dict(
            type="log", title="$n$", gridcolor="LightGray", gridwidth=0.5, showgrid=True
        ),
        yaxis=dict(
            type="log",
            title="Error",
            gridcolor="LightGray",
            gridwidth=0.5,
            showgrid=True,
        ),
    )

    fig.show()


if "__main__" == __name__:
    main()
