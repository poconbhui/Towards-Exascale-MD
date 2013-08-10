# scripts/relpath.rb
#
# This provides the relpath function which is useful for putting
# multifile scripts together without expecting them to be called
# from a particular directory.
#


# DEF relpath
#
# This function accepts a relative path and prepends the absolute path
# of the calling script to it.
#
def relpath(path)
    File.expand_path(path.to_s, File.dirname(__FILE__))
end
