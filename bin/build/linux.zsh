#!/usr/bin/env zsh

#################################
# Build script for Emacs on Linux
#
# This downloads the latest development version of Emacs into a temp directory, then builds it. The
# temp directory will be removed when the script exits.
#################################

echo "AERO --- Sudo is required for apt usage"
sudo -v

# Keep alive existing `sudo` time stamp until the script has finished.
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

echo "AERO --- Creating temporary work directory"
WORK_DIR=`mktemp -d`
if [[ ! "$WORK_DIR" || ! -d "$WORK_DIR" ]]; then
  echo "AERO --- Could not create temporary work directory"
  exit 1
fi

sudo chown -R "$USER" "$WORK_DIR"

echo "AERO --- Temporary work dir: ${WORK_DIR}"

function cleanup() {
  rm -rf "$WORK_DIR" || { echo "AERO --- Failed to remove temporary work directory, but this is okay, continuing..." }
}

# on exit signal, clean up
trap cleanup EXIT

### main

echo "AERO --- Installing requirements"

echo "AERO --- Getting Emacs source"
git clone https://git.savannah.gnu.org/git/emacs.git "$WORK_DIR" || { echo "AERO --- Failed to clone repository"; exit 1; }
cd "$WORK_DIR"

sudo apt update || { echo "AERO --- Failed to update apt"; exit 1; }

libs=(
  autoconf
  build-essential
  git
  gnutls-bin
  libgccjit0
  libgccjit-13-dev
  libgif-dev
  libglib2.0-dev
  libgnutls30
  libgtk-3-dev
  libgtk-4-dev
  libjansson-dev
  libjpeg-dev
  libncurses-dev
  libpng-dev
  libsqlite3-dev
  libtiff5-dev
  libtree-sitter-dev
  libx11-dev
  libxml2-dev
  libxpm-dev
  texinfo
)
sudo apt install -y ${libs[@]}

if [[ $? -ne 0 ]]; then
  print "Error: Apt install failed, check the output above for details"
  exit 1
fi

echo "AERO --- Done with requirements"

echo "AERO --- Configuring Emacs build in directory:"
pwd

# Configure our installation
#
# See https://git.savannah.gnu.org/cgit/emacs.git/tree/plain/INSTALL for latest info
#
# PKG_CONFIG_PATH must be set otherwise configure will fail to find some libraries.
#
# --with-native-compilation: enables ahead-of-time native compilation [this is SLOW up front]
# --with-json: enables native JSON support
# --with-threads: enables support for threads for elisp libraries
# --with-compress-install: compresses installation files to save space
# --with-modules: enables dynamic modules
# --with-tree-sitter: enables tree-sitter support
# --with-gnutls=ifavailable: enables gnutls support if they're already available
# --without-mailutils: disables mailutils support which we don't use
# --without-pop: disables pop support, which is insecure and unused
# CFLAGS: enables CPU optimizations, using native architecture

export PKG_CONFIG_PATH=/usr/lib/x86_64-linux-gnu/pkgconfig:/usr/share/pkgconfig
./autogen.sh && ./configure --with-native-compilation=aot --with-json --with-threads --with-compress-install --with-modules --with-tree-sitter --with-gnutls=ifavailable --without-mailutils --without-pop CFLAGS="-O3 -mtune=native -march=native -fomit-frame-pointer"

if [[ $? -ne 0 ]]; then
  print "Error: Configure failed, check the output above for details"
  exit 1
fi

# Verify that configure worked, sometimes it doesn't
if [[ ! -e "${WORK_DIR}/Makefile" ]]; then
  echo "Error: Configure script failed to create Makefile in ${WORK_DIR}. Contents of the directory:"
  exit 1
fi

echo "AERO --- Building Emacs"
make -j12 || { echo "AERO --- Failed to build Emacs"; exit 1; }
echo "AERO --- Installing Emacs"
sudo make install || { echo "AERO --- Failed to install Emacs"; exit 1; }
echo "AERO --- Cleaning up"
cleanup()
echo "AERO --- Done"
