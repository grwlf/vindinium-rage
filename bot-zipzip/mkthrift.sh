#!/bin/sh

cd `dirname $0`
thrift -gen hs -r thrift/tf.thrift
thrift -gen py -r thrift/tf.thrift
