#-----------------------------------------------------------------
# pycparser: c-cmment.py
#
# Example of how to extract and use comments information.
#
# Copyright (C) 2012
# License: BSD
#-----------------------------------------------------------------
import sys

# This is not required if you've installed pycparser into
# your site-packages/ with setup.py
#
sys.path.extend(['.', '..'])

from pycparser import c_parser, c_ast, parse_file

#-----------------------------------------------------------------
if __name__ == "__main__":
    import os
    if len(sys.argv) > 1:
        files  = sys.argv[1:]
    else:
        d= 'c_files'
        files = [os.path.join(d, fn)
                for fn in os.listdir('c_files')[:-1]]

    for cfn in files:
        ast = parse_file(cfn, use_cpp=True,
                cpp_args= [ "-C",       # trigger to keep comments      
                  '-D__builtin_va_list=int',
                  '-D__attribute__(a)=',
                ]
        )
        print "===== %s =====" % cfn 
        for fn, cmtLst in ast.commentDir.viewitems():
            for tkNo, (fl,fc), (tl, tc), comment in cmtLst:
                print "%d:%s:%d:%d-%d:%d: %s" % (
                   tkNo, fn, fl,fc, tl,tc, `comment`)
