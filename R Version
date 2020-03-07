#Com certeza:
sudo apt-get install gcc-multilib
sudo apt-get install gobjc++
sudo apt-get install liblzma-dev libblas-dev gfortran
sudo apt-get install fort77
sudo apt-get install build-essential
apt-get install f2c g++ gfortran gcc
sudo apt-get install xorg-dev #Esse precisa mesmo
sudo apt-get install aptitude
sudo aptitude install libreadline-dev
sudo apt-get install zlib-bin lzma
wget http://cran-r.c3sl.ufpr.br/src/base/R-3/R-3.0.2.tar.gz

#SACADA:
sudo bash -c 'echo "deb-src http://us.archive.ubuntu.com/ubuntu/ trusty
main restricted universe multiverse" >> /etc/apt/sources.list
apt-get build-dep r-base

./configure --prefix=/home/thiago/compilados/R-3.0.2
--x-includes=/usr/include/X11/ --x-libraries=/usr/lib/X11/
make
make check
make install

export PATH=$PATH:/home/thiago/compilados/R/bin
which R
