# scripts/relpath.rb
#
# This provides the relpath function which is useful for putting
# multifile scripts together without expecting them to be called
# from a particular directory.
# 
# Unfortunately, to reliably load it, relpath is needed.
# This script can by loaded by copying the body of relpath to the
# calling script with path.to_s replaced with the relative path to
# this script. Another approach is to just copy and paste relpath
# into the required script.
#


# DEF relpath
#
# This function accepts a relative path and prepends the absolute path
# of the calling script to it.
#
def relpath(path)
    File.expand_path(path.to_s, File.dirname(__FILE__))
end
