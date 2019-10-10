#!/bin/sh -e

##
## Download data for benchmarking with big files
##

#DOWNLOADER="curl --location"
DOWNLOADER="aria2c --max-connection-per-server=5 --continue"

OUTDIR=ex

WEBDIR=https://ftp.acc.umu.se/mirror/wikimedia.org/dumps/enwiki/20190901/

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

download () {
    file=$1
    echo "========== Downloading \"$file\" =========="
    $DOWNLOADER $WEBDIR/$file -o $OUTDIR/$file
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mkdir --parents $OUTDIR

#  46Mb:
#download "enwiki-20190901-pages-articles14.xml-p7697599p7744799.bz2"

# 624Mb:
#download "enwiki-20190901-pages-articles-multistream1.xml-p10p30302.bz2"

# 1.6Gb:
#download "enwiki-20190901-pages-meta-current6.xml-p565314p892912.bz2"

# 4Gb:
#download "enwiki-20190901-pages-meta-current24.xml-p30503451p32003451.bz2"

# 21GB:
#download "enwiki-20190901-pages-meta-history2.xml-p31255p31720.bz2"

#
# Special case, XML with a small tag contents
#
echo "Downloading 1HTQ"
$DOWNLOADER https://files.rcsb.org/download/1HTQ.xml.gz -o $OUTDIR/1HTQ.xml.gz
zcat $OUTDIR/1HTQ.xml.gz | bzip2 --fast > $OUTDIR/1HTQ.xml.bz2
rm $OUTDIR/1HTQ.xml.gz

