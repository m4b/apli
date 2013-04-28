out=`basename $1 .lhs` && lhs2TeX --poly $1 > $out.tex && xelatex $out.tex
