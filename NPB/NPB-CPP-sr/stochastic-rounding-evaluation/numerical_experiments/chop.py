import numpy as np


class ChopOptions:
    def __init__(
        self,
        format="h",
        subnormal=1,
        round=1,
        flip=0,
        p=0.5,
        explim=1,
        randfunc=None,
        params=None,
    ):
        self.format = format
        self.subnormal = subnormal
        self.round = round
        self.flip = flip
        self.p = p
        self.explim = explim
        self.randfunc = randfunc if randfunc else np.random.rand
        self.params = params


def chop(x, options=None):
    if not np.isreal(x).all():
        raise ValueError("Chop requires a real input array.")

    if options is None:
        options = ChopOptions()

    fpopts = options

    if fpopts.format in ["q43", "fp8-e4m3"]:
        t, emax = 4, 7
    elif fpopts.format in ["q52", "fp8-e5m2"]:
        t, emax = 3, 15
    elif fpopts.format in ["h", "half", "fp16"]:
        t, emax = 11, 15
    elif fpopts.format in ["b", "bfloat16"]:
        t, emax = 8, 127
    elif fpopts.format in ["s", "single", "fp32"]:
        t, emax = 24, 127
    elif fpopts.format in ["d", "double", "fp64"]:
        t, emax = 53, 1023
    elif fpopts.format == "c":
        if fpopts.params is None:
            raise ValueError('Must specify options.params with options.format = "c".')
        t, emax = fpopts.params
    else:
        raise ValueError("Unrecognized format.")

    emin = 1 - emax
    xmin = 2**emin
    emins = emin + 1 - t
    xmins = 2**emins
    xmax = 2**emax * (2 - 2 ** (1 - t))

    def roundit(y, fpopts):
        if fpopts.round == 1:
            return np.round(y)
        elif fpopts.round == 2:
            return np.ceil(y)
        elif fpopts.round == 3:
            return np.floor(y)
        elif fpopts.round == 4:
            return np.trunc(y)
        elif fpopts.round == 5:
            return np.floor(y) + (fpopts.randfunc(len(y)) < (y - np.floor(y)))
        elif fpopts.round == 6:
            return np.floor(y) + (fpopts.randfunc(len(y)) < 0.5)
        else:
            raise ValueError("Unrecognized rounding mode.")

    c = x.copy()
    e = np.floor(np.log2(np.abs(x)))
    ktemp = (e < emin) & (e >= emins)

    if fpopts.explim:
        k_sub = np.where(ktemp)
        k_norm = np.where(~ktemp)
    else:
        k_sub = []
        k_norm = np.arange(len(ktemp))

    c[k_norm] = np.ldexp(
        roundit(np.ldexp(x[k_norm], t - 1 - e[k_norm]), fpopts), e[k_norm] - (t - 1)
    )
    if k_sub:
        t1 = t - np.maximum(emin - e[k_sub], 0)
        c[k_sub] = np.ldexp(
            roundit(np.ldexp(x[k_sub], t1 - 1 - e[k_sub]), fpopts), e[k_sub] - (t1 - 1)
        )

    if fpopts.explim:
        if fpopts.round in [1, 6]:
            xboundary = 2**emax * (2 - (1 / 2) * 2 ** (1 - t))
            c[x >= xboundary] = np.inf
            c[x <= -xboundary] = -np.inf
        elif fpopts.round == 2:
            c[x > xmax] = np.inf
            c[(x < -xmax) & (x != -np.inf)] = -xmax
        elif fpopts.round == 3:
            c[(x > xmax) & (x != np.inf)] = xmax
            c[x < -xmax] = -np.inf
        elif fpopts.round in [4, 5]:
            c[(x > xmax) & (x != np.inf)] = xmax
            c[(x < -xmax) & (x != -np.inf)] = -xmax

        min_rep = xmin if fpopts.subnormal == 0 else xmins
        k_small = np.abs(c) < min_rep

        if fpopts.round == 1:
            k_round = k_small & (
                np.abs(c) >= min_rep / 2
                if fpopts.subnormal == 0
                else np.abs(c) > min_rep / 2
            )
            c[k_round] = np.sign(c[k_round]) * min_rep
            c[k_small & ~k_round] = 0
        elif fpopts.round == 2:
            k_round = k_small & (c > 0) & (c < min_rep)
            c[k_round] = min_rep
            c[k_small & ~k_round] = 0
        elif fpopts.round == 3:
            k_round = k_small & (c < 0) & (c > -min_rep)
            c[k_round] = -min_rep
            c[k_small & ~k_round] = 0
        elif fpopts.round in [4, 5, 6]:
            c[k_small] = 0

    return c, fpopts
