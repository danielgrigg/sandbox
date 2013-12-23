function unjar { echo "unjarring $1"; name=${1%%.jar}; name=${name##*.}; unzip $1 -d /tmp/$name; }
