
def diff-files [keyword] {
  fd $keyword | lines | where { $in | str ends-with ".md" } | difft $in.0 $in.1
}

def ed-files [keyword] {
  fd $keyword | lines | where { $in | str ends-with ".md" } | hx --vsplit $in.0 $in.1
}
