#!/bin/sh

APP=erlem
D=`date +'%y%m%d'`
VER=`date +'%y%m%d%H%M%S'`

ARC="erlem-$D.tar.gz"

#
# Make some dirs
#
mkdir -p rel
cd rel
rm -rf $D
mkdir $D
rm -f latest
ln -s $D latest
cd $D
#
# Copy applications
#
printf "#!/bin/sh\nerl -detached -pa */ebin -sname $APP@localhost -s $APP\n"  > start_bg.sh
printf "#!/bin/sh\nerl -pa */ebin -sname $APP@localhost -s $APP\n"  > start_fg.sh
chmod 1777 start.sh
for i in `ls ../../_build/default/lib`
do
    mkdir $i
    cp -r ../../_build/default/lib/$i/ebin $i
    if [ -e ../../_build/default/lib/$i/priv ] 
        then cp -r ../../_build/default/lib/$i/priv $i 
    fi
done
#
# make tarball
#
rm -rf $ARCH
tar czf ../$ARCH .
cd ..
rm -f erlem-latest.tar.gz
ln -s $ARC erlem-latest.tar.gz
