if [ -d build ]
then 
  rm -rf build
fi
mkdir build
echo "Compiling..."
javac -d build -cp .:jna.jar com/sun/jna/examples/HelloWorld.java 

