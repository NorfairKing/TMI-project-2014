filename=$(basename $2)
filename="${filename%.*}"
pdflatex --shell-escape $2 
makeglossaries "$filename"
makeindex "$filename"
pdflatex --shell-escape $2
bibtex "$filename"
makeglossaries "$filename"
makeindex "$filename"
pdflatex --shell-escape -interaction=$1 $2 
bibtex "$filename"
pdflatex --shell-escape -interaction=$1 $2 
pdflatex --shell-escape -interaction=$1 $2
