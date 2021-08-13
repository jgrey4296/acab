def build_slice(s, l, toks):
    first  = None
    second = None
    if 'first' in toks:
        first = toks['first'][0]
        result = first

    if 'second' in toks:
        second = toks['second'][0]

    return slice(first, second)
