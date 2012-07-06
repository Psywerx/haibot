#change name, cut

cat $1 | cut -b $2- | awk 'sub("..$", "")' > `basename $1`.db
