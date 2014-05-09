filename="$(basename $1)"
filename="${filename%.*}"
f1="$filename.svg"
f2="$filename.png"

rsvg-convert "$f1" -o "$f2"
