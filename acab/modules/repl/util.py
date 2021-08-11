def build_slice(s, l, toks):
    result = None
    first  = None
    second = None
    if 'first' in toks:
        first = toks['first']
        result = first

    if 'second' in toks:
        second = toks['second'][0]

    result = slice(first, second)

    return result
