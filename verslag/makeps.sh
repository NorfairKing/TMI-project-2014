filename=$(basename $2)
filename="${filename%.*}"
latex $2 
makeglossaries "$filename"
makeindex "$filename"
latex $2
bibtex "$filename"
makeglossaries "$filename"
makeindex "$filename"
latex -interaction=$1 $2 
bibtex "$filename"
latex -interaction=$1 $2 
latex -interaction=$1 $2
