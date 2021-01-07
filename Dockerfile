FROM fpco/stack-build-small

ENV XMONAD_DIR=/root/.xmonad

# Install dependencies
# See: https://github.com/xmonad/xmonad/blob/master/.github/workflows/tests.yml
RUN apt update
RUN apt install -y libasound2 libasound2-dev libxrandr-dev libtinfo-dev libx11-dev libgmp-dev libxss-dev libxft-dev

# Create ~/.xmonad
WORKDIR $XMONAD_DIR
COPY . $XMONAD_DIR

# Build xmonad
RUN stack build --fast --no-terminal

# Install xmonad
RUN ./build

# Check if xmonad binary is installed
RUN type xmonad
