#!/bin/bash

# Where to install the JDK
instDir="/usr/lib/jvm"

function usage {
    echo "Usage: update-jdk <path/to/new/jdk.tar.gz>"
}

jdkArchive=$1
version=$(basename "$jdkArchive")

# Check (somewhat) this is actually a JDK
if [ "${version:0:3}" != "jdk" ]; then
    echo "This doesn't look like a JDK. Expecting something named jdk-<version>.tar.gz."
    usage
    exit 1
fi

# Also, check this is the .tar.gz, not something else
if [[ ! "$version" =~ \.tar\.gz$ ]]; then
    echo "This doesn't look like .tar.gz file"
    usage
    exit 1
fi


# Extract archive and find jdk version (should be the first extracted directory)
tar xzf $jdkArchive
output=$(tar -tf $jdkArchive | head -1)

jdkDir=${output::-1}
jdkPath="${instDir}/$jdkDir"


# create directory
mkdir -p ${instDir}
mv "$jdkDir" ${instDir}


# A list of binaries to update
binaries=(java javac javaws)

for b in "${binaries[@]}"; do
    binary="/usr/bin/${b}"
    update-alternatives --install "${binary}" "${b}" "${jdkPath}/bin/${b}" 1
    chmod a+x "${binary}"
    update-alternatives --config ${b}
done

chown -R root:root ${instDir}/$jdkDir

echo "Don't forget to update JAVA_HOME. Add to your .bashrc:"
echo " export JAVA_HOME=${instDir}/$jdkDir"
