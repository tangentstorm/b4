#include <errno.h>
#include <poll.h>
#include <stdlib.h>
#include <unistd.h>

// One non-blocking read of stdin. An empty string means no bytes are
// waiting. A single EOT byte means the stream closed.

Term poll_stdin_run(Env e, Term* f, IoWork* w) {
  uint32_t max = (uint32_t)f[0];
  struct pollfd p;
  char* buf;
  ssize_t n;
  Term s;
  if (max == 0 || max > 4096) {
    max = 256;
  }
  p.fd = 0;
  p.events = POLLIN;
  p.revents = 0;
  if (poll(&p, 1, 0) < 0) {
    return io_str(e, "\004", 1);
  }
  if ((p.revents & POLLIN) == 0) {
    if (p.revents & (POLLHUP | POLLERR | POLLNVAL)) {
      return io_str(e, "\004", 1);
    }
    return io_str(e, "", 0);
  }
  buf = (char*)malloc((size_t)max + 1);
  n = read(0, buf, max);
  if (n <= 0) {
    free(buf);
    return io_str(e, "\004", 1);
  }
  s = io_str(e, buf, (uint64_t)n);
  free(buf);
  return s;
}

static void __attribute__((constructor)) poll_stdin_use(void) {
  io_eff(CID_POLL_STDIN, poll_stdin_run, 0);
}
