echo "Mounting"
sshfs user@192.168.0.24: /home/walt/tmp/osx_mount
echo "Copying initial"
cp app/* /home/walt/tmp/osx_mount/metaseq/app/
cp src/* /home/walt/tmp/osx_mount/metaseq/src/
echo "Listening"
ls src/*.hs | entr -r ./copy_to_osx.sh src /_ &
ls app/*.hs | entr -r ./copy_to_osx.sh app /_


