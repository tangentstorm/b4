function poll_stdin(max) {
  const sys = io_sys();
  const len = Math.min(Number(max) || 256, 4096);
  const b = new Uint8Array(Math.max(len, 1));
  const flags = sys.fcntl(0, 3, 0);
  if (flags >= 0) {
    sys.fcntl(0, 4, flags | 0x800);
  }
  const n = Number(sys.read(0, sys.ptr(b), len));
  if (n < 0) {
    return "";
  }
  if (n === 0) {
    return "\u0004";
  }
  return io_text(b, n);
}
