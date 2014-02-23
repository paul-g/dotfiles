# A simple script to plot total contributions to GitHub

# Set this up as a cronjob running daily around midnight (crontab -e):
# 55 23 * * * path/to/script fetch

USER="paul-g"
URL="https://github.com/${USER}"
BASE=`dirname $0`

function fetch {

    DATA="${BASE}/github-contrib.dat"
    LOG="${BASE}/github-contrib.log"

    date "+%d.%m.%g" >> ${LOG}
    contributions=`curl "${URL}" 2>> ${LOG} | grep -o '[0-9]* Total' | sed 's/ Total//'`
    echo "" >> ${LOG}

    d=`date "+%d.%m.%g"`

    echo "${d} ${contributions}" >> ${DATA}
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
	echo "${BASE}"
esac
