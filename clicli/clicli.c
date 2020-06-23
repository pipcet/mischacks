#include <poll.h>
#include <stdio.h>
#include <unistd.h>

int main(int argc, char **argv)
{
  while (1) {
    struct pollfd pfd[2] = {
      { 0, POLLIN, 0 },
      { 2, POLLIN, 0 },
    };

    poll(pfd, 2, -1);

    if (pfd[0].revents & POLLIN) {
      char c;
      write(1, &c, read(0, &c, 1));
    }
    if (pfd[1].revents & POLLIN) {
      char c;
      write(1, &c, read(2, &c, 1));
    }
  }

  return 0;
}
