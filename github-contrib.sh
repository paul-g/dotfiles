# A simple script to plot total contributions to GitHub

USER="paul-g"
URL="https://github.com/${USER}"
LOG="github-contrib.log"
DATA="github-contrib.dat"

function fetch {
    date "+%d.%m.%g" >> ${LOG}
    contributions=`curl "${URL}" 2>> ${LOG} | grep -o '[0-9]* Total' | sed 's/ Total//'`
    echo "" >> ${LOG}

    d=`date "+%d.%m.%g"`

    echo "${d} ${contributions}" >> github-contrib.dat
}

function plot {
    gnuplot << __EOF
set term pngcairo size 1920, 1080
set output "github-contrib.png"
set yrange [100:500]
set timefmt '%d.%m.%y'
set xdata time
set xrange ["01.01.14":"01.01.15"]
plot "github-contrib.dat" using 1:2 with lines
__EOF
    eog "github-contrib.png"
}

case $1 in
    fetch)
	fetch
	;;
    plot)
	plot
	;;
    *)
	echo "Usage: $0 <fetch|plot>"
esac
