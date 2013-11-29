import re

symbol_list = [
    "class ",
    "def ",
    "function! ",
    "defun ",
]

word_separators = r"0-9A-Za-z.-_"

def extract_symbol(haystack):
    """
    Return the symbol name for any symbols whose type is listed in symbol_list.

    >>> extract_symbol("class MikeClass(object):")
    'MikeClass'

    >>> extract_symbol("def silly_face():")
    'silly_face'

    >>> extract_symbol("function! MikeGrep()")
    'MikeGrep'

    >>> extract_symbol("(defun MikeExpand (arg)")
    'MikeExpand'
    """
    re_string = r"({})([{}]*)".format(
        "|".join([re.escape(w) for w in symbol_list]),
        word_separators,
    )
    re_string = "(" + re_string + ")(\w*)"
    try:
        return re.compile(re_string).search(haystack).groups()[2]
    except AttributeError, e:
        return None
    except IndexError, e:
        return None


if __name__ == "__main__":
    import doctest
    if doctest.testmod().failed == 0:
        print "All tests passed!"
