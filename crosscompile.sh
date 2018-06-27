#!/bin/bash
export OPENWRT_DIR=~/develop/hardware/openwrt-chaos_calmer
export TARGET_VER=mipsel_24kec+dsp_uClibc-0.9.33.2
export TOOLCH_VER=mipsel_24kec+dsp_gcc-4.8-linaro_uClibc-0.9.33.2
export ERLANG_VER=20.1
export ERLINT_VER=3.10

export STAGING_DIR=$OPENWRT_DIR/staging_dir
export TOOLCHAIN_DIR=$STAGING_DIR/toolchain-$TOOLCH_VER

export CFLAGS="-I$TOOLCHAIN_DIR/include"
export CXXFLAGS="-I$TOOLCHAIN_DIR/include"
export LDFLAGS="-L$TOOLCHAIN_DIR/lib -L$OPENWRT_DIR/build_dir/target-$TARGET_VER/otp_src_$ERLANG_VER/ipkg-install/usr/lib/erlang/usr/lib "
export LD_LIBRARY_PATH="$TOOLCHAIN_DIR/usr/lib:$OPENWRT_DIR/build_dir/target-$TARGET_VER/otp_src_$ERLANG_VER/ipkg-install/usr/lib/erlang/usr/lib"
export PATH="$TOOLCHAIN_DIR/bin:$PATH"
export ERL_CFLAGS="-I$OPENWRT_DIR/build_dir/target-$TARGET_VER/otp_src_$ERLANG_VER/ipkg-install/usr/lib/erlang/usr/include"
export ERL_LDFLAGS="-L$OPENWRT_DIR/build_dir/target-$TARGET_VER/otp_src_$ERLANG_VER/ipkg-install/usr/lib/erlang/lib/erl_interface-$ERLINT_VER/lib -L$OPENWRT_DIR/build_dir/target-$TARGET_VER/otp_src_$ERLANG_VER/ipkg-install/usr/lib/erlang/usr/lib -lerts_st -lei_st"

export CC=$TOOLCHAIN_DIR/bin/mipsel-openwrt-linux-gcc
export CXX=$TOOLCHAIN_DIR/bin/mipsel-openwrt-linux-c++
export LD=$TOOLCHAIN_DIR/bin/mipsel-openwrt-linux-ld

./rebar3 compile
