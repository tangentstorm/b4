function word_to_u32(w) {
  let x = 0;
  for (let i = 0; w.$ === "WCon"; i++) {
    x |= Number(w.head) << i;
    w = w.tail;
  }
  return x >>> 0;
}

function u32_to_word(x) {
  let w = {$: "WNil"};
  for (let i = 31; i >= 0; i--) {
    w = {$: "WCon", head: ((x >>> i) & 1) === 1, tail: w};
  }
  return w;
}

function cmp_new(a, b) {
  return {$: a < b ? "LT"
    : a === b ? "EQ" : "GT"};
}

function nat_divmod(a, b) {
  return b === 0n ? {$: "Tuple", fst: 0n, snd: a}
    : {$: "Tuple", fst: a / b, snd: a % b};
}

function nat_chk(n) {
  if (n > 281474976710655n) {
    throw "bend: a Nat past the largest immediate 2^48-1";
  }
  return n;
}

function f32_show(x) {
  if (x !== x) {
    return "nan";
  }
  if (!Number.isFinite(x) || Object.is(x, -0)) {
    return x < 0 ? "-inf"
      : x === 0 ? "-0" : "inf";
  }
  let s = "x";
  for (let p = 1; p <= 9 && Math.fround(Number(s)) !== x; p += 1) {
    s = String(Number(x.toExponential(p - 1)));
  }
  return s;
}

function f32_bits(x) {
  return new Uint32Array(new Float32Array([x]).buffer)[0];
}

function f32_from_bits(u) {
  return new Float32Array(new Uint32Array([u]).buffer)[0];
}

function f32_read(s) {
  const re = /^\s*[+-]?((\d+\.?\d*|\.\d+)(e[+-]?\d+)?|inf(inity)?|nan)$/i;
  const v = Number(s.replace(/inf\w*/i, "Infinity"));
  return re.test(s) ? {$: "Some", value: Math.fround(v)} : {$: "None"};
}

function char_new(code) {
  if (code > 0x10FFFF || (code >= 0xD800 && code <= 0xDFFF)) {
    throw "bend: " + code + " is not a Unicode scalar value";
  }
  return String.fromCodePoint(code);
}

// Array
// =====

function array_new(d, v) {
  if (d > 31n) {
    throw "bend: an array past the deepest block class 31";
  }
  return Array(2 ** Number(d)).fill(v);
}

// An unbalanced tree fails, as in C.
function array_node(a, b) {
  if (a.length !== b.length) {
    throw "bend: runtime fail-stop";
  }
  return a.concat(b);
}

function array_rmw(a, i, f) {
  const at = i % a.length;
  const old = a[at];
  a[at] = f(old);
  return {$: "Tuple", fst: a, snd: old};
}

// Run
// ===

function run_jump(f, x) {
  return {$: "$JMP", f: f, x: x};
}

function run_tail(f, x) {
  return {$: "$JMP", f: f.j?.f === f ? f.j : f, x: [x]};
}

function run_clo(j) {
  const f = (x) => run_loop(j(x));
  f.j = j;
  j.f = f;
  return f;
}

function run_loop(r) {
  while (r !== null && typeof r === "object" && r.$ === "$JMP") {
    r = r.f(...r.x);
  }
  return r;
}

function run_lib(f, n) {
  return (...a) => a.length < n ? run_lib((...b) => f(...a, ...b), n - a.length)
    : run_loop(f(...a));
}
const $0eff = {
...(() => {
// File
// ====

function file_open(path, mode) {
  const name = io_bytes(path);
  if (name.includes(0)) {
    return io_fail(process.platform === "darwin" ? 92 : 84);
  }
  if (!["r", "w", "a"].includes(mode)) {
    return io_fail(22);
  }
  try {
    const fd = require("fs")
      .openSync(name.length > 0 ? Buffer.from(name) : "", mode, 0o644);
    return io_done(fd);
  } catch (e) {
    return io_fail(-e.errno);
  }
}

return {
  ["file_open"]: { run: typeof file_open === "function" ? file_open : undefined, need: typeof file_open_need === "function" ? file_open_need : undefined },
};
})(),
...(() => {
// File
// ====

function file_close(file) {
  const fs = require("fs");
  try {
    fs.closeSync(file);
  } catch (e) {
  }
  return { $: "Unit" };
}

return {
  ["file_close"]: { run: typeof file_close === "function" ? file_close : undefined, need: typeof file_close_need === "function" ? file_close_need : undefined },
};
})(),
...(() => {
// File
// ====

function file_read_with(file, max, offset, pack) {
  const sys = io_sys();
  const len = Math.min(max, 2147483647);
  const b = new Uint8Array(Math.max(len, 1));
  const n = Number(offset === null ? sys.read(file, sys.ptr(b), len)
    : sys.pread(file, sys.ptr(b), len, BigInt(offset)));
  return io_tup(file, n < 0 ? io_fail(sys.errno()) : io_done(pack(b, n)));
}

function file_read_list(b, n) {
  let xs = { $: "Nil" };
  for (let i = n; i > 0; i -= 1) {
    xs = { $: "Con", head: b[i - 1], tail: xs };
  }
  return xs;
}

function file_read(file, max) {
  return file_read_with(file, max, null, io_text);
}

function file_read_bytes(file, max) {
  return file_read_with(file, max, null, file_read_list);
}

function file_read_at(file, offset, max) {
  return file_read_with(file, max, offset, file_read_list);
}

return {
  ["file_read"]: { run: typeof file_read === "function" ? file_read : undefined, need: typeof file_read_need === "function" ? file_read_need : undefined },
};
})(),
...(() => {
// IO
// ==

function io_write(text) {
  io_out(1, io_bytes(text));
  return { $: "Unit" };
}

return {
  ["io_write"]: { run: typeof io_write === "function" ? io_write : undefined, need: typeof io_write_need === "function" ? io_write_need : undefined },
};
})(),
};

// Program
// =======

function $main$() {
  return run_clo((_x_0) => {
  return run_jump($IO$bind$, [run_loop($IO$try$((_x_1) => $File$open$("/dev/stdin", "r", _x_1))), run_clo((_x_2) => {
  return run_jump($loop$, [{$: "Read"}, _x_2, run_loop($boot$()), ""]);
}), _x_0]);
});
}

function $IO$bind$(_m_0, _f_0, _k_0) {
  return run_tail(_m_0, run_clo((_x_0) => {
  return run_tail(_f_0(_x_0), _k_0);
}));
}

function $IO$try$(_act_0) {
  return run_clo((_x_0) => {
  return run_jump($IO$bind$, [_act_0, run_clo((_x_1) => {
  return run_jump($IO$pass$, [_x_1]);
}), _x_0]);
});
}

function $loop$(_input_0, _file_0, _shell_0, _pending_0) {
  if (_input_0.$ === "Read") {
    const _vm_0 = _shell_0["vm"];
    const _labels_0 = _shell_0["labels"];
    const _assembling_0 = _shell_0["assembling"];
    const _t_0 = _shell_0["quit"];
    if (_t_0) {
      const _report_0 = _shell_0["report"];
      return (_x_0) => $File$close$(_file_0, _x_0);
    } else {
      const _report_1 = _shell_0["report"];
      return run_clo((_x_1) => {
      return run_jump($IO$bind$, [(_x_2) => $File$read$(_file_0, 1, _x_2), run_clo((_x_3) => {
      return run_jump($vm$pair$, [_x_3, run_clo((_x_4) => {
      return run_clo((_x_5) => {
      return run_clo((_x_6) => {
      return run_jump($IO$bind$, [run_loop($IO$pass$(_x_5)), run_clo((_x_7) => {
      return run_jump($loop$, [{$: "Chunk", ["text"]: _x_7}, _x_4, {$: "Shell", ["vm"]: _vm_0, ["labels"]: _labels_0, ["assembling"]: _assembling_0, ["quit"]: _t_0, ["report"]: _report_1}, _pending_0]);
}), _x_6]);
});
});
})]);
}), _x_1]);
});
    }
  } else {
    const _t_1 = _input_0["text"];
    if (_t_1 === "") {
      return run_clo((_x_8) => {
      return run_jump($IO$bind$, [run_loop($flush$(run_loop($line$(run_loop($String$reverse$(_pending_0)), _shell_0)))), run_clo((_x_9) => {
      return (_x_10) => $File$close$(_file_0, _x_10);
}), _x_8]);
});
    } else {
      const _t_2 = (_t_1.codePointAt(0) > 0xFFFF ? _t_1.slice(0, 2) : _t_1[0]);
      const _t_3 = _t_2.codePointAt(0);
      if (_t_3 == 10) {
        const _tail_0 = (_t_1.codePointAt(0) > 0xFFFF ? _t_1.slice(2) : _t_1.slice(1));
        return run_clo((_x_11) => {
        return run_jump($IO$bind$, [run_loop($flush$(run_loop($line$(run_loop($String$reverse$(_pending_0)), _shell_0)))), run_clo((_x_12) => {
        return run_jump($loop$, [{$: "Read"}, _file_0, _x_12, ""]);
}), _x_11]);
});
      } else {
        const _71_0 = u32_to_word(_t_3)["head"];
        const _72_0 = u32_to_word(_t_3)["tail"];
        const _tail_1 = (_t_1.codePointAt(0) > 0xFFFF ? _t_1.slice(2) : _t_1.slice(1));
        return run_jump($loop$, [{$: "Read"}, _file_0, _shell_0, (char_new(word_to_u32({$: "WCon", ["head"]: _71_0, ["tail"]: _72_0})) + _pending_0)]);
      }
    }
  }
}

function $boot$() {
  return {$: "Shell", ["vm"]: run_loop($vm$boot$()), ["labels"]: run_loop($Map$new$()), ["assembling"]: false, ["quit"]: false, ["report"]: ""};
}

function $IO$pass$(_r_0) {
  if (_r_0.$ === "Done") {
    const _value_0 = _r_0["value"];
    return run_clo((_x_0) => {
    return run_jump($IO$pure$, [_value_0, _x_0]);
});
  } else {
    const _t_0 = _r_0["error"];
    const _code_0 = _t_0["fst"];
    const _message_0 = _t_0["snd"];
    return run_clo((_x_1) => {
    return run_jump($IO$die$, [_code_0, _message_0, _x_1]);
});
  }
}

function $vm$pair$(_p_0, _f_0) {
  const _a_0 = _p_0["fst"];
  const _b_0 = _p_0["snd"];
  return run_tail(_f_0(_a_0), _b_0);
}

function $flush$(_shell_0) {
  const _vm_0 = _shell_0["vm"];
  const _labels_0 = _shell_0["labels"];
  const _assembling_0 = _shell_0["assembling"];
  const _quit_0 = _shell_0["quit"];
  const _report_0 = _shell_0["report"];
  return run_jump($vm$state$, [_vm_0, run_clo((_x_0) => {
  return run_clo((_x_1) => {
  return run_clo((_x_2) => {
  return run_clo((_x_3) => {
  return run_clo((_x_4) => {
  return run_clo((_x_5) => {
  return run_clo((_x_6) => {
  const _x_7 = run_loop($Bool$pick$(run_loop($String$is_empty$(_x_5)), "", (_x_5 + "\n")));
  return run_jump($IO$bind$, [(_x_8) => $IO$write$((_report_0 + _x_7), _x_8), run_clo((_x_9) => {
  return run_clo((_x_10) => {
  return run_jump($IO$pure$, [{$: "Shell", ["vm"]: {$: "VM", ["mem"]: _x_0, ["ds"]: _x_1, ["cs"]: _x_2, ["ip"]: _x_3, ["control"]: _x_4, ["output"]: ""}, ["labels"]: _labels_0, ["assembling"]: _assembling_0, ["quit"]: _quit_0, ["report"]: ""}, _x_10]);
});
}), _x_6]);
});
});
});
});
});
});
})]);
}

function $line$(_text_0, _shell_0) {
  const _vm_0 = _shell_0["vm"];
  const _labels_0 = _shell_0["labels"];
  const _assembling_0 = _shell_0["assembling"];
  const _quit_0 = _shell_0["quit"];
  const _report_0 = _shell_0["report"];
  return run_jump($tokens$, [run_loop($String$split$(run_loop($text$normalize$(_text_0)), " ")), {$: "Shell", ["vm"]: _vm_0, ["labels"]: _labels_0, ["assembling"]: false, ["quit"]: _quit_0, ["report"]: _report_0}]);
}

function $String$reverse$(_s_0) {
  return run_jump($String$reverse$go$, [_s_0, ""]);
}

function $vm$boot$() {
  const _mem_0 = array_new(16n, 0);
  const _mem_1 = run_loop($vm$write_cell$(_mem_0, 124, 256));
  return {$: "VM", ["mem"]: _mem_1, ["ds"]: {$: "Nil"}, ["cs"]: {$: "Nil"}, ["ip"]: 256, ["control"]: {$: "Control", ["mode"]: {$: "Ti"}, ["fault"]: {$: "Clear"}, ["at"]: 0}, ["output"]: ""};
}

function $Map$new$() {
  return {$: "MTip"};
}

function $IO$pure$(_x_0, _k_0) {
  return run_tail(_k_0, _x_0);
}

function $IO$die$(_code_0, _msg_0, _k_0) {
  return {$: "Halt", ["code"]: _code_0, ["message"]: _msg_0};
}

function $vm$state$(_vm_0, _f_0) {
  const _mem_0 = _vm_0["mem"];
  const _ds_0 = _vm_0["ds"];
  const _cs_0 = _vm_0["cs"];
  const _ip_0 = _vm_0["ip"];
  const _control_0 = _vm_0["control"];
  const _output_0 = _vm_0["output"];
  return run_tail(_f_0(_mem_0)(_ds_0)(_cs_0)(_ip_0)(_control_0), _output_0);
}

function $Bool$pick$(_c_0, _a_0, _b_0) {
  if (!_c_0) {
    return _b_0;
  } else {
    return _a_0;
  }
}

function $String$is_empty$(_s_0) {
  if (_s_0 === "") {
    return true;
  } else {
    const _h_0 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(0, 2) : _s_0[0]);
    const _t_0 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(2) : _s_0.slice(1));
    return false;
  }
}

function $tokens$(_words_0, _shell_0) {
  if (_words_0.$ === "Nil") {
    return _shell_0;
  } else {
    const _h_0 = _words_0["head"];
    const _t_0 = _words_0["tail"];
    const _vm_0 = _shell_0["vm"];
    const _labels_0 = _shell_0["labels"];
    const _assembling_0 = _shell_0["assembling"];
    const _t_1 = _shell_0["quit"];
    if (_t_1) {
      const _report_0 = _shell_0["report"];
      return {$: "Shell", ["vm"]: _vm_0, ["labels"]: _labels_0, ["assembling"]: _assembling_0, ["quit"]: true, ["report"]: _report_0};
    } else {
      const _report_1 = _shell_0["report"];
      return run_jump($tokens$, [_t_0, run_loop($token$(_h_0, {$: "Shell", ["vm"]: _vm_0, ["labels"]: _labels_0, ["assembling"]: _assembling_0, ["quit"]: false, ["report"]: _report_1}))]);
    }
  }
}

function $String$split$(_s_0, _sep_0) {
  if (_s_0 === "") {
    return {$: "Con", ["head"]: "", ["tail"]: {$: "Nil"}};
  } else {
    const _h_0 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(0, 2) : _s_0[0]);
    const _t_0 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(2) : _s_0.slice(1));
    return run_jump($String$split$fin$, [_h_0, run_loop($String$split$(_t_0, _sep_0)), run_loop($Char$is_eq$(_h_0, _sep_0))]);
  }
}

function $text$normalize$(_s_0) {
  if (_s_0 === "") {
    return "";
  } else {
    const _t_0 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(0, 2) : _s_0[0]);
    const _t_1 = _t_0.codePointAt(0);
    if (_t_1 == 35) {
      const _t_2 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(2) : _s_0.slice(1));
      return "";
    } else {
      const _14_0 = u32_to_word(_t_1)["head"];
      const _15_0 = u32_to_word(_t_1)["tail"];
      const _t_3 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(2) : _s_0.slice(1));
      return (run_loop($Bool$pick$(run_loop($Char$is_space$(char_new(word_to_u32({$: "WCon", ["head"]: _14_0, ["tail"]: _15_0})))), " ", char_new(word_to_u32({$: "WCon", ["head"]: _14_0, ["tail"]: _15_0})))) + run_loop($text$normalize$(_t_3)));
    }
  }
}

function $String$reverse$go$(_s_0, _acc_0) {
  if (_s_0 === "") {
    return _acc_0;
  } else {
    const _h_0 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(0, 2) : _s_0[0]);
    const _t_0 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(2) : _s_0.slice(1));
    return run_jump($String$reverse$go$, [_t_0, (_h_0 + _acc_0)]);
  }
}

function $vm$write_cell$(_mem_0, _a_0, _v_0) {
  const _mem_1 = run_loop($vm$write_half$(_mem_0, _a_0, _v_0));
  return run_jump($vm$write_half$, [_mem_1, ((_a_0 + 2) >>> 0), (16n >= 32n ? 0 : (_v_0 >>> Number(16n)) >>> 0)]);
}

function $token$(_word_0, _shell_0) {
  if (_word_0 === "") {
    return _shell_0;
  } else {
    const _t_0 = (_word_0.codePointAt(0) > 0xFFFF ? _word_0.slice(0, 2) : _word_0[0]);
    const _t_1 = _t_0.codePointAt(0);
    if (_t_1 == 47) {
      const _t_2 = (_word_0.codePointAt(0) > 0xFFFF ? _word_0.slice(2) : _word_0.slice(1));
      if (_t_2 !== "") {
        const _t_3 = (_t_2.codePointAt(0) > 0xFFFF ? _t_2.slice(0, 2) : _t_2[0]);
        const _t_4 = _t_3.codePointAt(0);
        if (_t_4 == 113) {
          const _t_5 = (_t_2.codePointAt(0) > 0xFFFF ? _t_2.slice(2) : _t_2.slice(1));
          if (_t_5 === "") {
            const _vm_0 = _shell_0["vm"];
            const _labels_0 = _shell_0["labels"];
            const _assembling_0 = _shell_0["assembling"];
            const _quit_0 = _shell_0["quit"];
            const _report_0 = _shell_0["report"];
            return {$: "Shell", ["vm"]: _vm_0, ["labels"]: _labels_0, ["assembling"]: _assembling_0, ["quit"]: true, ["report"]: _report_0};
          } else {
            const _vm_1 = _shell_0["vm"];
            const _labels_1 = _shell_0["labels"];
            const _assembling_1 = _shell_0["assembling"];
            const _quit_1 = _shell_0["quit"];
            const _report_1 = _shell_0["report"];
            return run_jump($register_token$, [run_loop($reg_prefix$(47, ("q" + _t_5))), 47, ("/" + ("q" + _t_5)), _vm_1, _labels_1, _assembling_1, _quit_1, _report_1]);
          }
        } else if ((_t_4 & 3) == 1) {
          const _215_0 = u32_to_word(_t_4)["tail"]["tail"]["head"];
          const _216_0 = u32_to_word(_t_4)["tail"]["tail"]["tail"];
          const _208_0 = (_t_2.codePointAt(0) > 0xFFFF ? _t_2.slice(2) : _t_2.slice(1));
          const _vm_2 = _shell_0["vm"];
          const _labels_2 = _shell_0["labels"];
          const _assembling_2 = _shell_0["assembling"];
          const _quit_2 = _shell_0["quit"];
          const _report_2 = _shell_0["report"];
          return run_jump($register_token$, [run_loop($reg_prefix$(47, (char_new(word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _215_0, ["tail"]: _216_0}}})) + _208_0))), 47, ("/" + (char_new(word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _215_0, ["tail"]: _216_0}}})) + _208_0)), _vm_2, _labels_2, _assembling_2, _quit_2, _report_2]);
        } else if (_t_4 == 67) {
          const _t_6 = (_t_2.codePointAt(0) > 0xFFFF ? _t_2.slice(2) : _t_2.slice(1));
          if (_t_6 === "") {
            return run_jump($boot$, []);
          } else {
            const _vm_3 = _shell_0["vm"];
            const _labels_3 = _shell_0["labels"];
            const _assembling_3 = _shell_0["assembling"];
            const _quit_3 = _shell_0["quit"];
            const _report_3 = _shell_0["report"];
            return run_jump($register_token$, [run_loop($reg_prefix$(47, ("C" + _t_6))), 47, ("/" + ("C" + _t_6)), _vm_3, _labels_3, _assembling_3, _quit_3, _report_3]);
          }
        } else if (_t_4 == 115) {
          const _t_7 = (_t_2.codePointAt(0) > 0xFFFF ? _t_2.slice(2) : _t_2.slice(1));
          if (_t_7 === "") {
            const _vm_4 = _shell_0["vm"];
            const _labels_4 = _shell_0["labels"];
            const _assembling_4 = _shell_0["assembling"];
            const _quit_4 = _shell_0["quit"];
            const _report_4 = _shell_0["report"];
            return {$: "Shell", ["vm"]: run_loop($single_step$(_vm_4)), ["labels"]: _labels_4, ["assembling"]: false, ["quit"]: _quit_4, ["report"]: _report_4};
          } else {
            const _vm_5 = _shell_0["vm"];
            const _labels_5 = _shell_0["labels"];
            const _assembling_5 = _shell_0["assembling"];
            const _quit_5 = _shell_0["quit"];
            const _report_5 = _shell_0["report"];
            return run_jump($register_token$, [run_loop($reg_prefix$(47, ("s" + _t_7))), 47, ("/" + ("s" + _t_7)), _vm_5, _labels_5, _assembling_5, _quit_5, _report_5]);
          }
        } else if ((_t_4 & 7) == 3) {
          const _277_0 = u32_to_word(_t_4)["tail"]["tail"]["tail"]["head"];
          const _278_0 = u32_to_word(_t_4)["tail"]["tail"]["tail"]["tail"];
          const _208_1 = (_t_2.codePointAt(0) > 0xFFFF ? _t_2.slice(2) : _t_2.slice(1));
          const _vm_6 = _shell_0["vm"];
          const _labels_6 = _shell_0["labels"];
          const _assembling_6 = _shell_0["assembling"];
          const _quit_6 = _shell_0["quit"];
          const _report_6 = _shell_0["report"];
          return run_jump($register_token$, [run_loop($reg_prefix$(47, (char_new(word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _277_0, ["tail"]: _278_0}}}})) + _208_1))), 47, ("/" + (char_new(word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _277_0, ["tail"]: _278_0}}}})) + _208_1)), _vm_6, _labels_6, _assembling_6, _quit_6, _report_6]);
        } else if (_t_4 == 111) {
          const _t_8 = (_t_2.codePointAt(0) > 0xFFFF ? _t_2.slice(2) : _t_2.slice(1));
          if (_t_8 === "") {
            const _vm_7 = _shell_0["vm"];
            const _labels_7 = _shell_0["labels"];
            const _assembling_7 = _shell_0["assembling"];
            const _quit_7 = _shell_0["quit"];
            const _report_7 = _shell_0["report"];
            return {$: "Shell", ["vm"]: run_loop($vm$set_mode$(_vm_7, {$: "Vo"})), ["labels"]: _labels_7, ["assembling"]: false, ["quit"]: _quit_7, ["report"]: _report_7};
          } else {
            const _vm_8 = _shell_0["vm"];
            const _labels_8 = _shell_0["labels"];
            const _assembling_8 = _shell_0["assembling"];
            const _quit_8 = _shell_0["quit"];
            const _report_8 = _shell_0["report"];
            return run_jump($register_token$, [run_loop($reg_prefix$(47, ("o" + _t_8))), 47, ("/" + ("o" + _t_8)), _vm_8, _labels_8, _assembling_8, _quit_8, _report_8]);
          }
        } else if ((_t_4 & 15) == 15) {
          const _391_0 = u32_to_word(_t_4)["tail"]["tail"]["tail"]["tail"]["head"];
          const _392_0 = u32_to_word(_t_4)["tail"]["tail"]["tail"]["tail"]["tail"];
          const _208_2 = (_t_2.codePointAt(0) > 0xFFFF ? _t_2.slice(2) : _t_2.slice(1));
          const _vm_9 = _shell_0["vm"];
          const _labels_9 = _shell_0["labels"];
          const _assembling_9 = _shell_0["assembling"];
          const _quit_9 = _shell_0["quit"];
          const _report_9 = _shell_0["report"];
          return run_jump($register_token$, [run_loop($reg_prefix$(47, (char_new(word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _391_0, ["tail"]: _392_0}}}}})) + _208_2))), 47, ("/" + (char_new(word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _391_0, ["tail"]: _392_0}}}}})) + _208_2)), _vm_9, _labels_9, _assembling_9, _quit_9, _report_9]);
        } else if (_t_4 == 103) {
          const _t_9 = (_t_2.codePointAt(0) > 0xFFFF ? _t_2.slice(2) : _t_2.slice(1));
          if (_t_9 === "") {
            const _vm_10 = _shell_0["vm"];
            const _labels_10 = _shell_0["labels"];
            const _assembling_10 = _shell_0["assembling"];
            const _quit_10 = _shell_0["quit"];
            const _report_10 = _shell_0["report"];
            return {$: "Shell", ["vm"]: run_loop($resume$(_vm_10)), ["labels"]: _labels_10, ["assembling"]: false, ["quit"]: _quit_10, ["report"]: _report_10};
          } else {
            const _vm_11 = _shell_0["vm"];
            const _labels_11 = _shell_0["labels"];
            const _assembling_11 = _shell_0["assembling"];
            const _quit_11 = _shell_0["quit"];
            const _report_11 = _shell_0["report"];
            return run_jump($register_token$, [run_loop($reg_prefix$(47, ("g" + _t_9))), 47, ("/" + ("g" + _t_9)), _vm_11, _labels_11, _assembling_11, _quit_11, _report_11]);
          }
        } else if ((_t_4 & 15) == 7) {
          const _447_0 = u32_to_word(_t_4)["tail"]["tail"]["tail"]["tail"]["head"];
          const _448_0 = u32_to_word(_t_4)["tail"]["tail"]["tail"]["tail"]["tail"];
          const _208_3 = (_t_2.codePointAt(0) > 0xFFFF ? _t_2.slice(2) : _t_2.slice(1));
          const _vm_12 = _shell_0["vm"];
          const _labels_12 = _shell_0["labels"];
          const _assembling_12 = _shell_0["assembling"];
          const _quit_12 = _shell_0["quit"];
          const _report_12 = _shell_0["report"];
          return run_jump($register_token$, [run_loop($reg_prefix$(47, (char_new(word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _447_0, ["tail"]: _448_0}}}}})) + _208_3))), 47, ("/" + (char_new(word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _447_0, ["tail"]: _448_0}}}}})) + _208_3)), _vm_12, _labels_12, _assembling_12, _quit_12, _report_12]);
        } else if (_t_4 == 116) {
          const _t_10 = (_t_2.codePointAt(0) > 0xFFFF ? _t_2.slice(2) : _t_2.slice(1));
          if (_t_10 === "") {
            const _vm_13 = _shell_0["vm"];
            const _labels_13 = _shell_0["labels"];
            const _assembling_13 = _shell_0["assembling"];
            const _quit_13 = _shell_0["quit"];
            const _report_13 = _shell_0["report"];
            return {$: "Shell", ["vm"]: run_loop($vm$set_mode$(_vm_13, {$: "Ti"})), ["labels"]: _labels_13, ["assembling"]: false, ["quit"]: _quit_13, ["report"]: _report_13};
          } else {
            const _vm_14 = _shell_0["vm"];
            const _labels_14 = _shell_0["labels"];
            const _assembling_14 = _shell_0["assembling"];
            const _quit_14 = _shell_0["quit"];
            const _report_14 = _shell_0["report"];
            return run_jump($register_token$, [run_loop($reg_prefix$(47, ("t" + _t_10))), 47, ("/" + ("t" + _t_10)), _vm_14, _labels_14, _assembling_14, _quit_14, _report_14]);
          }
        } else {
          const _503_0 = u32_to_word(_t_4)["tail"]["head"];
          const _504_0 = u32_to_word(_t_4)["tail"]["tail"];
          const _208_4 = (_t_2.codePointAt(0) > 0xFFFF ? _t_2.slice(2) : _t_2.slice(1));
          const _vm_15 = _shell_0["vm"];
          const _labels_15 = _shell_0["labels"];
          const _assembling_15 = _shell_0["assembling"];
          const _quit_15 = _shell_0["quit"];
          const _report_15 = _shell_0["report"];
          return run_jump($register_token$, [run_loop($reg_prefix$(47, (char_new(word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _503_0, ["tail"]: _504_0}})) + _208_4))), 47, ("/" + (char_new(word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _503_0, ["tail"]: _504_0}})) + _208_4)), _vm_15, _labels_15, _assembling_15, _quit_15, _report_15]);
        }
      } else {
        const _vm_16 = _shell_0["vm"];
        const _labels_16 = _shell_0["labels"];
        const _assembling_16 = _shell_0["assembling"];
        const _quit_16 = _shell_0["quit"];
        const _report_16 = _shell_0["report"];
        return run_jump($register_token$, [run_loop($reg_prefix$(47, _t_2)), 47, ("/" + _t_2), _vm_16, _labels_16, _assembling_16, _quit_16, _report_16]);
      }
    } else if (_t_1 == 63) {
      const _140_0 = (_word_0.codePointAt(0) > 0xFFFF ? _word_0.slice(2) : _word_0.slice(1));
      const _vm_17 = _shell_0["vm"];
      const _labels_17 = _shell_0["labels"];
      const _assembling_17 = _shell_0["assembling"];
      const _quit_17 = _shell_0["quit"];
      const _report_17 = _shell_0["report"];
      return run_jump($vm$pair$, [run_loop($query$(_140_0, _vm_17)), run_clo((_x_0) => {
      return run_clo((_x_1) => {
      return {$: "Shell", ["vm"]: _x_0, ["labels"]: _labels_17, ["assembling"]: false, ["quit"]: _quit_17, ["report"]: (_report_17 + _x_1)};
});
})]);
    } else if (_t_1 == 39) {
      const _t_11 = (_word_0.codePointAt(0) > 0xFFFF ? _word_0.slice(2) : _word_0.slice(1));
      if (_t_11 === "") {
        const _vm_18 = _shell_0["vm"];
        const _labels_18 = _shell_0["labels"];
        const _assembling_18 = _shell_0["assembling"];
        const _quit_18 = _shell_0["quit"];
        const _report_18 = _shell_0["report"];
        return {$: "Shell", ["vm"]: run_loop($number$(_assembling_18, _vm_18, 32)), ["labels"]: _labels_18, ["assembling"]: _assembling_18, ["quit"]: _quit_18, ["report"]: _report_18};
      } else {
        const _t_12 = (_t_11.codePointAt(0) > 0xFFFF ? _t_11.slice(0, 2) : _t_11[0]);
        const _ch_0 = _t_12.codePointAt(0);
        const _t_13 = (_t_11.codePointAt(0) > 0xFFFF ? _t_11.slice(2) : _t_11.slice(1));
        if (_t_13 === "") {
          const _vm_19 = _shell_0["vm"];
          const _labels_19 = _shell_0["labels"];
          const _assembling_19 = _shell_0["assembling"];
          const _quit_19 = _shell_0["quit"];
          const _report_19 = _shell_0["report"];
          return {$: "Shell", ["vm"]: run_loop($number$(_assembling_19, _vm_19, _ch_0)), ["labels"]: _labels_19, ["assembling"]: _assembling_19, ["quit"]: _quit_19, ["report"]: _report_19};
        } else {
          const _vm_20 = _shell_0["vm"];
          const _labels_20 = _shell_0["labels"];
          const _assembling_20 = _shell_0["assembling"];
          const _quit_20 = _shell_0["quit"];
          const _report_20 = _shell_0["report"];
          return run_jump($register_token$, [run_loop($reg_prefix$(39, (char_new(_ch_0) + _t_13))), 39, ("'" + (char_new(_ch_0) + _t_13)), _vm_20, _labels_20, _assembling_20, _quit_20, _report_20]);
        }
      }
    } else if ((_t_1 & 1) == 1) {
      const _145_0 = u32_to_word(_t_1)["tail"]["head"];
      const _146_0 = u32_to_word(_t_1)["tail"]["tail"];
      const _140_1 = (_word_0.codePointAt(0) > 0xFFFF ? _word_0.slice(2) : _word_0.slice(1));
      const _vm_21 = _shell_0["vm"];
      const _labels_21 = _shell_0["labels"];
      const _assembling_21 = _shell_0["assembling"];
      const _quit_21 = _shell_0["quit"];
      const _report_21 = _shell_0["report"];
      return run_jump($register_token$, [run_loop($reg_prefix$(word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _145_0, ["tail"]: _146_0}}), _140_1)), word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _145_0, ["tail"]: _146_0}}), (char_new(word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _145_0, ["tail"]: _146_0}})) + _140_1), _vm_21, _labels_21, _assembling_21, _quit_21, _report_21]);
    } else if (_t_1 == 58) {
      const _t_14 = (_word_0.codePointAt(0) > 0xFFFF ? _word_0.slice(2) : _word_0.slice(1));
      if (_t_14 === "") {
        const _vm_22 = _shell_0["vm"];
        const _labels_22 = _shell_0["labels"];
        const _assembling_22 = _shell_0["assembling"];
        const _quit_22 = _shell_0["quit"];
        const _report_22 = _shell_0["report"];
        return {$: "Shell", ["vm"]: _vm_22, ["labels"]: _labels_22, ["assembling"]: true, ["quit"]: _quit_22, ["report"]: _report_22};
      } else {
        const _vm_23 = _shell_0["vm"];
        const _labels_23 = _shell_0["labels"];
        const _assembling_23 = _shell_0["assembling"];
        const _quit_23 = _shell_0["quit"];
        const _report_23 = _shell_0["report"];
        return run_jump($define$, [run_loop($text$register$(_t_14)), _t_14, _vm_23, _labels_23, _quit_23, _report_23]);
      }
    } else {
      const _677_0 = u32_to_word(_t_1)["tail"]["head"];
      const _678_0 = u32_to_word(_t_1)["tail"]["tail"];
      const _140_2 = (_word_0.codePointAt(0) > 0xFFFF ? _word_0.slice(2) : _word_0.slice(1));
      const _vm_24 = _shell_0["vm"];
      const _labels_24 = _shell_0["labels"];
      const _assembling_24 = _shell_0["assembling"];
      const _quit_24 = _shell_0["quit"];
      const _report_24 = _shell_0["report"];
      return run_jump($register_token$, [run_loop($reg_prefix$(word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _677_0, ["tail"]: _678_0}}), _140_2)), word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _677_0, ["tail"]: _678_0}}), (char_new(word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _677_0, ["tail"]: _678_0}})) + _140_2), _vm_24, _labels_24, _assembling_24, _quit_24, _report_24]);
    }
  }
}

function $String$split$fin$(_c_0, _r_0, _cut_0) {
  if (!_cut_0) {
    return run_jump($String$split$push$, [_c_0, _r_0]);
  } else {
    return {$: "Con", ["head"]: "", ["tail"]: _r_0};
  }
}

function $Char$is_eq$(_a_0, _b_0) {
  const _x_0 = _a_0.codePointAt(0);
  const _y_0 = _b_0.codePointAt(0);
  return (_x_0 === _y_0);
}

function $Char$is_space$(_c_0) {
  const _x_0 = _c_0.codePointAt(0);
  const _x_1 = (_x_0 === 32);
  const _x_2 = run_loop($Bool$and$((_x_0 >= 9), (_x_0 <= 13)));
  return (_x_1 || _x_2);
}

function $vm$write_half$(_mem_0, _a_0, _v_0) {
  const _lo_0 = run_loop($vm$low_byte$(_v_0));
  const _hi_0 = run_loop($vm$high_byte$(_v_0));
  const _mem_1 = (_mem_0[_a_0 % _mem_0.length] = _lo_0, _mem_0);
  const _x_0 = ((_a_0 + 1) >>> 0);
  return (_mem_1[_x_0 % _mem_1.length] = _hi_0, _mem_1);
}

function $register_token$(_reg_0, _prefix_0, _name_0, _vm_0, _labels_0, _assembling_0, _quit_0, _report_0) {
  if (_reg_0.$ === "None") {
    return run_jump($ordinary$, [_name_0, _vm_0, _labels_0, _assembling_0, _quit_0, _report_0]);
  } else {
    const _r_0 = _reg_0["value"];
    return {$: "Shell", ["vm"]: run_loop($reg_operation$(_prefix_0, _r_0, _assembling_0, _vm_0)), ["labels"]: _labels_0, ["assembling"]: _assembling_0, ["quit"]: _quit_0, ["report"]: _report_0};
  }
}

function $reg_prefix$(_prefix_0, _tail_0) {
  if (_prefix_0 == 96) {
    return run_jump($text$register$, [_tail_0]);
  } else if (_prefix_0 == 64) {
    return run_jump($text$register$, [_tail_0]);
  } else if ((_prefix_0 & 3) == 0) {
    const _13_0 = u32_to_word(_prefix_0)["tail"]["tail"]["head"];
    const _14_0 = u32_to_word(_prefix_0)["tail"]["tail"]["tail"];
    return {$: "None"};
  } else if (_prefix_0 == 94) {
    return run_jump($text$register$, [_tail_0]);
  } else if ((_prefix_0 & 3) == 2) {
    const _125_0 = u32_to_word(_prefix_0)["tail"]["tail"]["head"];
    const _126_0 = u32_to_word(_prefix_0)["tail"]["tail"]["tail"];
    return {$: "None"};
  } else if (_prefix_0 == 33) {
    return run_jump($text$register$, [_tail_0]);
  } else if ((_prefix_0 & 3) == 1) {
    const _187_0 = u32_to_word(_prefix_0)["tail"]["tail"]["head"];
    const _188_0 = u32_to_word(_prefix_0)["tail"]["tail"]["tail"];
    return {$: "None"};
  } else if (_prefix_0 == 43) {
    return run_jump($text$register$, [_tail_0]);
  } else {
    const _247_0 = u32_to_word(_prefix_0)["tail"]["tail"]["head"];
    const _248_0 = u32_to_word(_prefix_0)["tail"]["tail"]["tail"];
    return {$: "None"};
  }
}

function $single_step$(_vm_0) {
  return run_jump($vm$step$, [run_loop($vm$set_mode$(_vm_0, {$: "Ti"}))]);
}

function $vm$set_mode$(_vm_0, _mode_0) {
  const _mem_0 = _vm_0["mem"];
  const _ds_0 = _vm_0["ds"];
  const _cs_0 = _vm_0["cs"];
  const _ip_0 = _vm_0["ip"];
  const _control_0 = _vm_0["control"];
  const _output_0 = _vm_0["output"];
  return {$: "VM", ["mem"]: _mem_0, ["ds"]: _ds_0, ["cs"]: _cs_0, ["ip"]: _ip_0, ["control"]: {$: "Control", ["mode"]: _mode_0, ["fault"]: {$: "Clear"}, ["at"]: 0}, ["output"]: _output_0};
}

function $resume$(_vm_0) {
  const _mem_0 = _vm_0["mem"];
  const _ds_0 = _vm_0["ds"];
  const _cs_0 = _vm_0["cs"];
  const _ip_0 = _vm_0["ip"];
  const _control_0 = _vm_0["control"];
  const _output_0 = _vm_0["output"];
  return run_jump($vm$run$, [BigInt(1000000), {$: "VM", ["mem"]: _mem_0, ["ds"]: _ds_0, ["cs"]: _cs_0, ["ip"]: _ip_0, ["control"]: {$: "Control", ["mode"]: {$: "Ru"}, ["fault"]: {$: "Clear"}, ["at"]: 0}, ["output"]: _output_0}]);
}

function $query$(_name_0, _vm_0) {
  if (_name_0 !== "") {
    const _t_0 = (_name_0.codePointAt(0) > 0xFFFF ? _name_0.slice(0, 2) : _name_0[0]);
    const _t_1 = _t_0.codePointAt(0);
    if (_t_1 == 100) {
      const _t_2 = (_name_0.codePointAt(0) > 0xFFFF ? _name_0.slice(2) : _name_0.slice(1));
      if (_t_2 === "") {
        return run_jump($vm$state$, [_vm_0, run_clo((_x_0) => {
        return run_clo((_x_1) => {
        return run_clo((_x_2) => {
        return run_clo((_x_3) => {
        return run_clo((_x_4) => {
        return run_clo((_x_5) => {
        return run_jump($vm$pair$, [run_loop($text$stack$(_x_1)), run_clo((_x_6) => {
        return run_clo((_x_7) => {
        const _x_8 = (_x_7 + "]\n");
        return {$: "Tuple", ["fst"]: {$: "VM", ["mem"]: _x_0, ["ds"]: _x_6, ["cs"]: _x_2, ["ip"]: _x_3, ["control"]: _x_4, ["output"]: _x_5}, ["snd"]: ("ds: [" + _x_8)};
});
})]);
});
});
});
});
});
})]);
      } else {
        return run_jump($query_register$, [run_loop($text$register$(("d" + _t_2))), ("d" + _t_2), _vm_0]);
      }
    } else if ((_t_1 & 1) == 0) {
      const _78_0 = u32_to_word(_t_1)["tail"]["head"];
      const _79_0 = u32_to_word(_t_1)["tail"]["tail"];
      const _73_0 = (_name_0.codePointAt(0) > 0xFFFF ? _name_0.slice(2) : _name_0.slice(1));
      return run_jump($query_register$, [run_loop($text$register$((char_new(word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _78_0, ["tail"]: _79_0}})) + _73_0))), (char_new(word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _78_0, ["tail"]: _79_0}})) + _73_0), _vm_0]);
    } else if (_t_1 == 99) {
      const _t_3 = (_name_0.codePointAt(0) > 0xFFFF ? _name_0.slice(2) : _name_0.slice(1));
      if (_t_3 === "") {
        return run_jump($vm$state$, [_vm_0, run_clo((_x_9) => {
        return run_clo((_x_10) => {
        return run_clo((_x_11) => {
        return run_clo((_x_12) => {
        return run_clo((_x_13) => {
        return run_clo((_x_14) => {
        return run_jump($vm$pair$, [run_loop($text$stack$(_x_11)), run_clo((_x_15) => {
        return run_clo((_x_16) => {
        const _x_17 = (_x_16 + "]\n");
        return {$: "Tuple", ["fst"]: {$: "VM", ["mem"]: _x_9, ["ds"]: _x_10, ["cs"]: _x_15, ["ip"]: _x_12, ["control"]: _x_13, ["output"]: _x_14}, ["snd"]: ("cs: [" + _x_17)};
});
})]);
});
});
});
});
});
})]);
      } else {
        return run_jump($query_register$, [run_loop($text$register$(("c" + _t_3))), ("c" + _t_3), _vm_0]);
      }
    } else if (_t_1 == 115) {
      const _t_4 = (_name_0.codePointAt(0) > 0xFFFF ? _name_0.slice(2) : _name_0.slice(1));
      if (_t_4 === "") {
        const _mem_0 = _vm_0["mem"];
        const _ds_0 = _vm_0["ds"];
        const _cs_0 = _vm_0["cs"];
        const _ip_0 = _vm_0["ip"];
        const _control_0 = _vm_0["control"];
        const _output_0 = _vm_0["output"];
        const _x_18 = run_loop($control_text$(_control_0));
        const _x_19 = (_x_18 + "\n");
        return {$: "Tuple", ["fst"]: {$: "VM", ["mem"]: _mem_0, ["ds"]: _ds_0, ["cs"]: _cs_0, ["ip"]: _ip_0, ["control"]: _control_0, ["output"]: _output_0}, ["snd"]: ("state: " + _x_19)};
      } else {
        return run_jump($query_register$, [run_loop($text$register$(("s" + _t_4))), ("s" + _t_4), _vm_0]);
      }
    } else if ((_t_1 & 3) == 3) {
      const _142_0 = u32_to_word(_t_1)["tail"]["tail"]["head"];
      const _143_0 = u32_to_word(_t_1)["tail"]["tail"]["tail"];
      const _73_1 = (_name_0.codePointAt(0) > 0xFFFF ? _name_0.slice(2) : _name_0.slice(1));
      return run_jump($query_register$, [run_loop($text$register$((char_new(word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _142_0, ["tail"]: _143_0}}})) + _73_1))), (char_new(word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _142_0, ["tail"]: _143_0}}})) + _73_1), _vm_0]);
    } else if (_t_1 == 105) {
      const _t_5 = (_name_0.codePointAt(0) > 0xFFFF ? _name_0.slice(2) : _name_0.slice(1));
      if (_t_5 === "") {
        const _mem_1 = _vm_0["mem"];
        const _ds_1 = _vm_0["ds"];
        const _cs_1 = _vm_0["cs"];
        const _ip_1 = _vm_0["ip"];
        const _control_1 = _vm_0["control"];
        const _output_1 = _vm_0["output"];
        const _x_20 = run_loop($text$hex$(_ip_1));
        const _x_21 = (_x_20 + "\n");
        return {$: "Tuple", ["fst"]: {$: "VM", ["mem"]: _mem_1, ["ds"]: _ds_1, ["cs"]: _cs_1, ["ip"]: _ip_1, ["control"]: _control_1, ["output"]: _output_1}, ["snd"]: ("ip: " + _x_21)};
      } else {
        return run_jump($query_register$, [run_loop($text$register$(("i" + _t_5))), ("i" + _t_5), _vm_0]);
      }
    } else {
      const _256_0 = u32_to_word(_t_1)["tail"]["tail"]["head"];
      const _257_0 = u32_to_word(_t_1)["tail"]["tail"]["tail"];
      const _73_2 = (_name_0.codePointAt(0) > 0xFFFF ? _name_0.slice(2) : _name_0.slice(1));
      return run_jump($query_register$, [run_loop($text$register$((char_new(word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _256_0, ["tail"]: _257_0}}})) + _73_2))), (char_new(word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _256_0, ["tail"]: _257_0}}})) + _73_2), _vm_0]);
    }
  } else {
    return run_jump($query_register$, [run_loop($text$register$(_name_0)), _name_0, _vm_0]);
  }
}

function $number$(_assembling_0, _vm_0, _value_0) {
  if (_assembling_0) {
    return run_jump($emit$, [_vm_0, _value_0]);
  } else {
    return run_jump($vm$push$, [_vm_0, _value_0]);
  }
}

function $define$(_reg_0, _name_0, _vm_0, _labels_0, _quit_0, _report_0) {
  if (_reg_0.$ === "Some") {
    const _r_0 = _reg_0["value"];
    return run_jump($define_named$, [_name_0, {$: "Some", ["value"]: _r_0}, _vm_0, _labels_0, _quit_0, _report_0]);
  } else {
    return run_jump($define_number$, [run_loop($text$read_hex$(_name_0)), _name_0, _vm_0, _labels_0, _quit_0, _report_0]);
  }
}

function $text$register$(_s_0) {
  if (_s_0 !== "") {
    const _t_0 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(0, 2) : _s_0[0]);
    const _c_0 = _t_0.codePointAt(0);
    const _t_1 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(2) : _s_0.slice(1));
    if (_t_1 === "") {
      return run_jump($text$parsed$, [run_loop($Bool$and$((_c_0 >= 64), (_c_0 <= 95))), ((_c_0 - 64) >>> 0)]);
    } else {
      return {$: "None"};
    }
  } else {
    return {$: "None"};
  }
}

function $String$split$push$(_c_0, _ps_0) {
  if (_ps_0.$ === "Nil") {
    return {$: "Con", ["head"]: (_c_0 + ""), ["tail"]: {$: "Nil"}};
  } else {
    const _h_0 = _ps_0["head"];
    const _t_0 = _ps_0["tail"];
    return {$: "Con", ["head"]: (_c_0 + _h_0), ["tail"]: _t_0};
  }
}

function $Bool$and$(_a_0, _b_0) {
  if (!_a_0) {
    return false;
  } else {
    return _b_0;
  }
}

function $vm$low_byte$(_x_0) {
  return ((_x_0 & 255) >>> 0);
}

function $vm$high_byte$(_x_0) {
  const _x_1 = (8n >= 32n ? 0 : (_x_0 >>> Number(8n)) >>> 0);
  return ((_x_1 & 255) >>> 0);
}

function $ordinary$(_name_0, _vm_0, _labels_0, _assembling_0, _quit_0, _report_0) {
  const _op_0 = run_loop($text$opcode$(_name_0));
  return run_jump($ordinary_op$, [(_op_0 < 256), _op_0, _name_0, _vm_0, _labels_0, _assembling_0, _quit_0, _report_0]);
}

function $reg_operation$(_prefix_0, _r_0, _assembling_0, _vm_0) {
  if (_prefix_0 == 96) {
    return run_jump($number$, [_assembling_0, _vm_0, (Math.imul(_r_0, 4) >>> 0)]);
  } else if (_prefix_0 == 64) {
    return run_jump($operation$, [_assembling_0, _vm_0, ((32 + _r_0) >>> 0)]);
  } else if ((_prefix_0 & 3) == 0) {
    const _19_0 = u32_to_word(_prefix_0)["tail"]["tail"]["head"];
    const _20_0 = u32_to_word(_prefix_0)["tail"]["tail"]["tail"];
    return _vm_0;
  } else if (_prefix_0 == 94) {
    return run_jump($invoke_or_emit$, [_assembling_0, _vm_0, _r_0]);
  } else if ((_prefix_0 & 3) == 2) {
    const _131_0 = u32_to_word(_prefix_0)["tail"]["tail"]["head"];
    const _132_0 = u32_to_word(_prefix_0)["tail"]["tail"]["tail"];
    return _vm_0;
  } else if (_prefix_0 == 33) {
    return run_jump($operation$, [_assembling_0, _vm_0, ((64 + _r_0) >>> 0)]);
  } else if ((_prefix_0 & 3) == 1) {
    const _193_0 = u32_to_word(_prefix_0)["tail"]["tail"]["head"];
    const _194_0 = u32_to_word(_prefix_0)["tail"]["tail"]["tail"];
    return _vm_0;
  } else if (_prefix_0 == 43) {
    return run_jump($operation$, [_assembling_0, _vm_0, ((96 + _r_0) >>> 0)]);
  } else {
    const _253_0 = u32_to_word(_prefix_0)["tail"]["tail"]["head"];
    const _254_0 = u32_to_word(_prefix_0)["tail"]["tail"]["tail"];
    return _vm_0;
  }
}

function $vm$step$(_vm_0) {
  const _mem_0 = _vm_0["mem"];
  const _ds_0 = _vm_0["ds"];
  const _cs_0 = _vm_0["cs"];
  const _ip_0 = _vm_0["ip"];
  const _control_0 = _vm_0["control"];
  const _output_0 = _vm_0["output"];
  return run_jump($vm$fetch$, [run_loop($vm$code_ok$(_ip_0)), {$: "VM", ["mem"]: _mem_0, ["ds"]: _ds_0, ["cs"]: _cs_0, ["ip"]: _ip_0, ["control"]: _control_0, ["output"]: _output_0}]);
}

function $vm$run$(_fuel_0, _vm_0) {
  if (_fuel_0 === 0n) {
    return _vm_0;
  } else {
    const _left_0 = (_fuel_0 - 1n);
    const _mem_0 = _vm_0["mem"];
    const _ds_0 = _vm_0["ds"];
    const _cs_0 = _vm_0["cs"];
    const _ip_0 = _vm_0["ip"];
    const _t_0 = _vm_0["control"];
    const _t_1 = _t_0["mode"];
    if (_t_1.$ === "Ru") {
      const _reason_0 = _t_0["fault"];
      const _at_0 = _t_0["at"];
      const _output_0 = _vm_0["output"];
      return run_jump($vm$run$, [_left_0, run_loop($vm$tick$({$: "VM", ["mem"]: _mem_0, ["ds"]: _ds_0, ["cs"]: _cs_0, ["ip"]: _ip_0, ["control"]: {$: "Control", ["mode"]: {$: "Ru"}, ["fault"]: _reason_0, ["at"]: _at_0}, ["output"]: _output_0}))]);
    } else {
      const _reason_1 = _t_0["fault"];
      const _at_1 = _t_0["at"];
      const _output_1 = _vm_0["output"];
      return {$: "VM", ["mem"]: _mem_0, ["ds"]: _ds_0, ["cs"]: _cs_0, ["ip"]: _ip_0, ["control"]: {$: "Control", ["mode"]: _t_1, ["fault"]: _reason_1, ["at"]: _at_1}, ["output"]: _output_1};
    }
  }
}

function $text$stack$(_xs_0) {
  return run_jump($text$stack_go$, [_xs_0, ""]);
}

function $query_register$(_reg_0, _name_0, _vm_0) {
  if (_reg_0.$ === "None") {
    return run_jump($query_address$, [run_loop($text$read_hex$(_name_0)), _vm_0]);
  } else {
    const _r_0 = _reg_0["value"];
    return run_jump($vm$state$, [_vm_0, run_clo((_x_0) => {
    return run_clo((_x_1) => {
    return run_clo((_x_2) => {
    return run_clo((_x_3) => {
    return run_clo((_x_4) => {
    return run_clo((_x_5) => {
    return run_jump($vm$pair$, [run_loop($vm$read_cell$(_x_0, (Math.imul(_r_0, 4) >>> 0))), run_clo((_x_6) => {
    return run_clo((_x_7) => {
    const _x_8 = run_loop($text$hex_fixed$(8n, _x_7, ""));
    return {$: "Tuple", ["fst"]: {$: "VM", ["mem"]: _x_6, ["ds"]: _x_1, ["cs"]: _x_2, ["ip"]: _x_3, ["control"]: _x_4, ["output"]: _x_5}, ["snd"]: (_x_8 + "\n")};
});
})]);
});
});
});
});
});
})]);
  }
}

function $control_text$(_control_0) {
  const _t_0 = _control_0["mode"];
  if (_t_0.$ === "Vo") {
    const _reason_0 = _control_0["fault"];
    const _at_0 = _control_0["at"];
    return "vo";
  } else if (_t_0.$ === "Ru") {
    const _reason_1 = _control_0["fault"];
    const _at_1 = _control_0["at"];
    return "ru";
  } else if (_t_0.$ === "Na") {
    const _reason_2 = _control_0["fault"];
    const _at_2 = _control_0["at"];
    const _x_0 = run_loop($text$hex$(_at_2));
    const _x_1 = run_loop($fault_text$(_reason_2));
    const _x_2 = (" at " + _x_0);
    const _x_3 = (_x_1 + _x_2);
    return ("na " + _x_3);
  } else {
    const _reason_3 = _control_0["fault"];
    const _at_3 = _control_0["at"];
    return "ti";
  }
}

function $text$hex$(_x_0) {
  return run_jump($text$trim_zero$, [run_loop($text$hex_fixed$(8n, _x_0, ""))]);
}

function $emit$(_vm_0, _value_0) {
  return run_jump($vm$state$, [_vm_0, run_clo((_x_0) => {
  return run_clo((_x_1) => {
  return run_clo((_x_2) => {
  return run_clo((_x_3) => {
  return run_clo((_x_4) => {
  return run_clo((_x_5) => {
  return run_jump($vm$pair$, [run_loop($vm$read_cell$(_x_0, 124)), run_clo((_x_6) => {
  return run_clo((_x_7) => {
  const _x_8 = run_loop($vm$low_byte$(_value_0));
  const _mem_0 = (_x_6[_x_7 % _x_6.length] = _x_8, _x_6);
  const _mem_1 = run_loop($vm$write_cell$(_mem_0, 124, ((_x_7 + 1) >>> 0)));
  return {$: "VM", ["mem"]: _mem_1, ["ds"]: _x_1, ["cs"]: _x_2, ["ip"]: _x_3, ["control"]: _x_4, ["output"]: _x_5};
});
})]);
});
});
});
});
});
})]);
}

function $vm$push$(_vm_0, _x_0) {
  const _mem_0 = _vm_0["mem"];
  const _ds_0 = _vm_0["ds"];
  const _cs_0 = _vm_0["cs"];
  const _ip_0 = _vm_0["ip"];
  const _control_0 = _vm_0["control"];
  const _output_0 = _vm_0["output"];
  return {$: "VM", ["mem"]: _mem_0, ["ds"]: {$: "Con", ["head"]: _x_0, ["tail"]: _ds_0}, ["cs"]: _cs_0, ["ip"]: _ip_0, ["control"]: _control_0, ["output"]: _output_0};
}

function $define_named$(_name_0, _reg_0, _vm_0, _labels_0, _quit_0, _report_0) {
  if (_reg_0.$ === "Some") {
    const _r_0 = _reg_0["value"];
    return run_jump($vm$state$, [_vm_0, run_clo((_x_0) => {
    return run_clo((_x_1) => {
    return run_clo((_x_2) => {
    return run_clo((_x_3) => {
    return run_clo((_x_4) => {
    return run_clo((_x_5) => {
    return run_jump($vm$pair$, [run_loop($vm$read_cell$(_x_0, 124)), run_clo((_x_6) => {
    return run_clo((_x_7) => {
    const _mem_0 = run_loop($vm$write_cell$(_x_6, (Math.imul(_r_0, 4) >>> 0), _x_7));
    return {$: "Shell", ["vm"]: {$: "VM", ["mem"]: _mem_0, ["ds"]: _x_1, ["cs"]: _x_2, ["ip"]: _x_3, ["control"]: _x_4, ["output"]: _x_5}, ["labels"]: _labels_0, ["assembling"]: true, ["quit"]: _quit_0, ["report"]: _report_0};
});
})]);
});
});
});
});
});
})]);
  } else {
    return run_jump($vm$state$, [_vm_0, run_clo((_x_8) => {
    return run_clo((_x_9) => {
    return run_clo((_x_10) => {
    return run_clo((_x_11) => {
    return run_clo((_x_12) => {
    return run_clo((_x_13) => {
    return run_jump($vm$pair$, [run_loop($vm$read_cell$(_x_8, 124)), run_clo((_x_14) => {
    return run_clo((_x_15) => {
    return {$: "Shell", ["vm"]: {$: "VM", ["mem"]: _x_14, ["ds"]: _x_9, ["cs"]: _x_10, ["ip"]: _x_11, ["control"]: _x_12, ["output"]: _x_13}, ["labels"]: run_loop($Map$set$(_labels_0, _name_0, _x_15)), ["assembling"]: true, ["quit"]: _quit_0, ["report"]: _report_0};
});
})]);
});
});
});
});
});
})]);
  }
}

function $define_number$(_number_0, _name_0, _vm_0, _labels_0, _quit_0, _report_0) {
  if (_number_0.$ === "Some") {
    const _a_0 = _number_0["value"];
    return {$: "Shell", ["vm"]: run_loop($set_here$(_vm_0, _a_0)), ["labels"]: _labels_0, ["assembling"]: true, ["quit"]: _quit_0, ["report"]: _report_0};
  } else {
    return run_jump($define_named$, [_name_0, run_loop($text$register$(_name_0)), _vm_0, _labels_0, _quit_0, _report_0]);
  }
}

function $text$read_hex$(_s_0) {
  if (_s_0 === "") {
    return {$: "None"};
  } else {
    const _t_0 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(0, 2) : _s_0[0]);
    const _t_1 = _t_0.codePointAt(0);
    if (_t_1 == 45) {
      const _t_2 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(2) : _s_0.slice(1));
      if (_t_2 === "") {
        return {$: "None"};
      } else {
        return run_jump($text$negative_hex$, [run_loop($text$read_hex_go$(_t_2, 0, true))]);
      }
    } else {
      const _17_0 = u32_to_word(_t_1)["head"];
      const _18_0 = u32_to_word(_t_1)["tail"];
      const _14_0 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(2) : _s_0.slice(1));
      return run_jump($text$read_hex_go$, [(char_new(word_to_u32({$: "WCon", ["head"]: _17_0, ["tail"]: _18_0})) + _14_0), 0, true]);
    }
  }
}

function $text$parsed$(_ok_0, _x_0) {
  if (_ok_0) {
    return {$: "Some", ["value"]: _x_0};
  } else {
    return {$: "None"};
  }
}

function $text$opcode$(_s_0) {
  if (_s_0 !== "") {
    const _t_0 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(0, 2) : _s_0[0]);
    const _t_1 = _t_0.codePointAt(0);
    if (_t_1 == 46) {
      const _t_2 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(2) : _s_0.slice(1));
      if (_t_2 !== "") {
        const _t_3 = (_t_2.codePointAt(0) > 0xFFFF ? _t_2.slice(0, 2) : _t_2[0]);
        const _t_4 = _t_3.codePointAt(0);
        if (_t_4 == 46) {
          const _t_5 = (_t_2.codePointAt(0) > 0xFFFF ? _t_2.slice(2) : _t_2.slice(1));
          if (_t_5 === "") {
            return 0;
          } else {
            return run_jump($text$find_name$, [run_loop($text$core_names$()), ("." + ("." + _t_5)), 128]);
          }
        } else {
          const _79_0 = u32_to_word(_t_4)["head"];
          const _80_0 = u32_to_word(_t_4)["tail"];
          const _76_0 = (_t_2.codePointAt(0) > 0xFFFF ? _t_2.slice(2) : _t_2.slice(1));
          return run_jump($text$find_name$, [run_loop($text$core_names$()), ("." + (char_new(word_to_u32({$: "WCon", ["head"]: _79_0, ["tail"]: _80_0})) + _76_0)), 128]);
        }
      } else {
        return run_jump($text$find_name$, [run_loop($text$core_names$()), ("." + _t_2), 128]);
      }
    } else if (_t_1 == 110) {
      const _t_6 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(2) : _s_0.slice(1));
      if (_t_6 !== "") {
        const _t_7 = (_t_6.codePointAt(0) > 0xFFFF ? _t_6.slice(0, 2) : _t_6[0]);
        const _t_8 = _t_7.codePointAt(0);
        if (_t_8 == 49) {
          const _t_9 = (_t_6.codePointAt(0) > 0xFFFF ? _t_6.slice(2) : _t_6.slice(1));
          if (_t_9 === "") {
            return 247;
          } else {
            return run_jump($text$find_name$, [run_loop($text$core_names$()), ("n" + ("1" + _t_9)), 128]);
          }
        } else {
          const _197_0 = u32_to_word(_t_8)["head"];
          const _198_0 = u32_to_word(_t_8)["tail"];
          const _194_0 = (_t_6.codePointAt(0) > 0xFFFF ? _t_6.slice(2) : _t_6.slice(1));
          return run_jump($text$find_name$, [run_loop($text$core_names$()), ("n" + (char_new(word_to_u32({$: "WCon", ["head"]: _197_0, ["tail"]: _198_0})) + _194_0)), 128]);
        }
      } else {
        return run_jump($text$find_name$, [run_loop($text$core_names$()), ("n" + _t_6), 128]);
      }
    } else if ((_t_1 & 3) == 2) {
      const _15_0 = u32_to_word(_t_1)["tail"]["tail"]["head"];
      const _16_0 = u32_to_word(_t_1)["tail"]["tail"]["tail"];
      const _8_0 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(2) : _s_0.slice(1));
      return run_jump($text$find_name$, [run_loop($text$core_names$()), (char_new(word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _15_0, ["tail"]: _16_0}}})) + _8_0), 128]);
    } else if (_t_1 == 100) {
      const _t_10 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(2) : _s_0.slice(1));
      if (_t_10 !== "") {
        const _t_11 = (_t_10.codePointAt(0) > 0xFFFF ? _t_10.slice(0, 2) : _t_10[0]);
        const _t_12 = _t_11.codePointAt(0);
        if (_t_12 == 98) {
          const _t_13 = (_t_10.codePointAt(0) > 0xFFFF ? _t_10.slice(2) : _t_10.slice(1));
          if (_t_13 === "") {
            return 254;
          } else {
            return run_jump($text$find_name$, [run_loop($text$core_names$()), ("d" + ("b" + _t_13)), 128]);
          }
        } else {
          const _325_0 = u32_to_word(_t_12)["head"];
          const _326_0 = u32_to_word(_t_12)["tail"];
          const _322_0 = (_t_10.codePointAt(0) > 0xFFFF ? _t_10.slice(2) : _t_10.slice(1));
          return run_jump($text$find_name$, [run_loop($text$core_names$()), ("d" + (char_new(word_to_u32({$: "WCon", ["head"]: _325_0, ["tail"]: _326_0})) + _322_0)), 128]);
        }
      } else {
        return run_jump($text$find_name$, [run_loop($text$core_names$()), ("d" + _t_10), 128]);
      }
    } else if ((_t_1 & 7) == 4) {
      const _263_0 = u32_to_word(_t_1)["tail"]["tail"]["tail"]["head"];
      const _264_0 = u32_to_word(_t_1)["tail"]["tail"]["tail"]["tail"];
      const _8_1 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(2) : _s_0.slice(1));
      return run_jump($text$find_name$, [run_loop($text$core_names$()), (char_new(word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _263_0, ["tail"]: _264_0}}}})) + _8_1), 128]);
    } else if (_t_1 == 104) {
      const _t_14 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(2) : _s_0.slice(1));
      if (_t_14 !== "") {
        const _t_15 = (_t_14.codePointAt(0) > 0xFFFF ? _t_14.slice(0, 2) : _t_14[0]);
        const _t_16 = _t_15.codePointAt(0);
        if (_t_16 == 108) {
          const _t_17 = (_t_14.codePointAt(0) > 0xFFFF ? _t_14.slice(2) : _t_14.slice(1));
          if (_t_17 === "") {
            return 255;
          } else {
            return run_jump($text$find_name$, [run_loop($text$core_names$()), ("h" + ("l" + _t_17)), 128]);
          }
        } else {
          const _451_0 = u32_to_word(_t_16)["head"];
          const _452_0 = u32_to_word(_t_16)["tail"];
          const _448_0 = (_t_14.codePointAt(0) > 0xFFFF ? _t_14.slice(2) : _t_14.slice(1));
          return run_jump($text$find_name$, [run_loop($text$core_names$()), ("h" + (char_new(word_to_u32({$: "WCon", ["head"]: _451_0, ["tail"]: _452_0})) + _448_0)), 128]);
        }
      } else {
        return run_jump($text$find_name$, [run_loop($text$core_names$()), ("h" + _t_14), 128]);
      }
    } else if ((_t_1 & 7) == 0) {
      const _389_0 = u32_to_word(_t_1)["tail"]["tail"]["tail"]["head"];
      const _390_0 = u32_to_word(_t_1)["tail"]["tail"]["tail"]["tail"];
      const _8_2 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(2) : _s_0.slice(1));
      return run_jump($text$find_name$, [run_loop($text$core_names$()), (char_new(word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _389_0, ["tail"]: _390_0}}}})) + _8_2), 128]);
    } else if (_t_1 == 99) {
      const _t_18 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(2) : _s_0.slice(1));
      if (_t_18 !== "") {
        const _t_19 = (_t_18.codePointAt(0) > 0xFFFF ? _t_18.slice(0, 2) : _t_18[0]);
        const _t_20 = _t_19.codePointAt(0);
        if (_t_20 == 48) {
          const _t_21 = (_t_18.codePointAt(0) > 0xFFFF ? _t_18.slice(2) : _t_18.slice(1));
          if (_t_21 === "") {
            return 192;
          } else {
            return run_jump($text$find_name$, [run_loop($text$core_names$()), ("c" + ("0" + _t_21)), 128]);
          }
        } else if ((_t_20 & 7) == 0) {
          const _587_0 = u32_to_word(_t_20)["tail"]["tail"]["tail"]["head"];
          const _588_0 = u32_to_word(_t_20)["tail"]["tail"]["tail"]["tail"];
          const _578_0 = (_t_18.codePointAt(0) > 0xFFFF ? _t_18.slice(2) : _t_18.slice(1));
          return run_jump($text$find_name$, [run_loop($text$core_names$()), ("c" + (char_new(word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _587_0, ["tail"]: _588_0}}}})) + _578_0)), 128]);
        } else if (_t_20 == 52) {
          const _t_22 = (_t_18.codePointAt(0) > 0xFFFF ? _t_18.slice(2) : _t_18.slice(1));
          if (_t_22 === "") {
            return 248;
          } else {
            return run_jump($text$find_name$, [run_loop($text$core_names$()), ("c" + ("4" + _t_22)), 128]);
          }
        } else if ((_t_20 & 7) == 4) {
          const _645_0 = u32_to_word(_t_20)["tail"]["tail"]["tail"]["head"];
          const _646_0 = u32_to_word(_t_20)["tail"]["tail"]["tail"]["tail"];
          const _578_1 = (_t_18.codePointAt(0) > 0xFFFF ? _t_18.slice(2) : _t_18.slice(1));
          return run_jump($text$find_name$, [run_loop($text$core_names$()), ("c" + (char_new(word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _645_0, ["tail"]: _646_0}}}})) + _578_1)), 128]);
        } else if (_t_20 == 50) {
          const _t_23 = (_t_18.codePointAt(0) > 0xFFFF ? _t_18.slice(2) : _t_18.slice(1));
          if (_t_23 === "") {
            return 246;
          } else {
            return run_jump($text$find_name$, [run_loop($text$core_names$()), ("c" + ("2" + _t_23)), 128]);
          }
        } else if ((_t_20 & 3) == 2) {
          const _703_0 = u32_to_word(_t_20)["tail"]["tail"]["head"];
          const _704_0 = u32_to_word(_t_20)["tail"]["tail"]["tail"];
          const _578_2 = (_t_18.codePointAt(0) > 0xFFFF ? _t_18.slice(2) : _t_18.slice(1));
          return run_jump($text$find_name$, [run_loop($text$core_names$()), ("c" + (char_new(word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _703_0, ["tail"]: _704_0}}})) + _578_2)), 128]);
        } else if (_t_20 == 49) {
          const _t_24 = (_t_18.codePointAt(0) > 0xFFFF ? _t_18.slice(2) : _t_18.slice(1));
          if (_t_24 === "") {
            return 193;
          } else {
            return run_jump($text$find_name$, [run_loop($text$core_names$()), ("c" + ("1" + _t_24)), 128]);
          }
        } else {
          const _763_0 = u32_to_word(_t_20)["tail"]["head"];
          const _764_0 = u32_to_word(_t_20)["tail"]["tail"];
          const _578_3 = (_t_18.codePointAt(0) > 0xFFFF ? _t_18.slice(2) : _t_18.slice(1));
          return run_jump($text$find_name$, [run_loop($text$core_names$()), ("c" + (char_new(word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _763_0, ["tail"]: _764_0}})) + _578_3)), 128]);
        }
      } else {
        return run_jump($text$find_name$, [run_loop($text$core_names$()), ("c" + _t_18), 128]);
      }
    } else if ((_t_1 & 3) == 3) {
      const _517_0 = u32_to_word(_t_1)["tail"]["tail"]["head"];
      const _518_0 = u32_to_word(_t_1)["tail"]["tail"]["tail"];
      const _8_3 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(2) : _s_0.slice(1));
      return run_jump($text$find_name$, [run_loop($text$core_names$()), (char_new(word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _517_0, ["tail"]: _518_0}}})) + _8_3), 128]);
    } else if (_t_1 == 105) {
      const _t_25 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(2) : _s_0.slice(1));
      if (_t_25 !== "") {
        const _t_26 = (_t_25.codePointAt(0) > 0xFFFF ? _t_25.slice(0, 2) : _t_25[0]);
        const _t_27 = _t_26.codePointAt(0);
        if (_t_27 == 111) {
          const _t_28 = (_t_25.codePointAt(0) > 0xFFFF ? _t_25.slice(2) : _t_25.slice(1));
          if (_t_28 === "") {
            return 253;
          } else {
            return run_jump($text$find_name$, [run_loop($text$core_names$()), ("i" + ("o" + _t_28)), 128]);
          }
        } else {
          const _889_0 = u32_to_word(_t_27)["head"];
          const _890_0 = u32_to_word(_t_27)["tail"];
          const _886_0 = (_t_25.codePointAt(0) > 0xFFFF ? _t_25.slice(2) : _t_25.slice(1));
          return run_jump($text$find_name$, [run_loop($text$core_names$()), ("i" + (char_new(word_to_u32({$: "WCon", ["head"]: _889_0, ["tail"]: _890_0})) + _886_0)), 128]);
        }
      } else {
        return run_jump($text$find_name$, [run_loop($text$core_names$()), ("i" + _t_25), 128]);
      }
    } else {
      const _825_0 = u32_to_word(_t_1)["tail"]["tail"]["head"];
      const _826_0 = u32_to_word(_t_1)["tail"]["tail"]["tail"];
      const _8_4 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(2) : _s_0.slice(1));
      return run_jump($text$find_name$, [run_loop($text$core_names$()), (char_new(word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _825_0, ["tail"]: _826_0}}})) + _8_4), 128]);
    }
  } else {
    return run_jump($text$find_name$, [run_loop($text$core_names$()), _s_0, 128]);
  }
}

function $ordinary_op$(_known_0, _op_0, _name_0, _vm_0, _labels_0, _assembling_0, _quit_0, _report_0) {
  if (_known_0) {
    return {$: "Shell", ["vm"]: run_loop($operation$(_assembling_0, _vm_0, _op_0)), ["labels"]: _labels_0, ["assembling"]: _assembling_0, ["quit"]: _quit_0, ["report"]: _report_0};
  } else {
    return run_jump($literal$, [run_loop($text$read_hex$(_name_0)), _name_0, _vm_0, _labels_0, _assembling_0, _quit_0, _report_0]);
  }
}

function $operation$(_assembling_0, _vm_0, _op_0) {
  if (_assembling_0) {
    return run_jump($emit$, [_vm_0, _op_0]);
  } else {
    return run_jump($calculator$, [_vm_0, _op_0]);
  }
}

function $invoke_or_emit$(_assembling_0, _vm_0, _r_0) {
  if (_assembling_0) {
    return run_jump($emit$, [_vm_0, _r_0]);
  } else {
    return run_jump($invoke_register$, [_vm_0, _r_0]);
  }
}

function $vm$fetch$(_ok_0, _vm_0) {
  if (!_ok_0) {
    return run_jump($vm$fault$, [_vm_0, {$: "Address"}]);
  } else {
    const _mem_0 = _vm_0["mem"];
    const _ds_0 = _vm_0["ds"];
    const _cs_0 = _vm_0["cs"];
    const _ip_0 = _vm_0["ip"];
    const _control_0 = _vm_0["control"];
    const _output_0 = _vm_0["output"];
    return run_jump($vm$pair$, [{$: "Tuple", fst: _mem_0, snd: _mem_0[_ip_0 % _mem_0.length]}, run_clo((_x_0) => {
    return run_clo((_x_1) => {
    return run_jump($vm$advance$, [run_loop($vm$execute$(_x_1, {$: "VM", ["mem"]: _x_0, ["ds"]: _ds_0, ["cs"]: _cs_0, ["ip"]: _ip_0, ["control"]: _control_0, ["output"]: _output_0}))]);
});
})]);
  }
}

function $vm$code_ok$(_addr_0) {
  return run_jump($Bool$and$, [(_addr_0 >= 256), (_addr_0 < 65536)]);
}

function $vm$tick$(_vm_0) {
  const _mem_0 = _vm_0["mem"];
  const _ds_0 = _vm_0["ds"];
  const _cs_0 = _vm_0["cs"];
  const _ip_0 = _vm_0["ip"];
  const _t_0 = _vm_0["control"];
  const _t_1 = _t_0["mode"];
  if (_t_1.$ === "Ru") {
    const _reason_0 = _t_0["fault"];
    const _at_0 = _t_0["at"];
    const _output_0 = _vm_0["output"];
    return run_jump($vm$step$, [{$: "VM", ["mem"]: _mem_0, ["ds"]: _ds_0, ["cs"]: _cs_0, ["ip"]: _ip_0, ["control"]: {$: "Control", ["mode"]: {$: "Ru"}, ["fault"]: _reason_0, ["at"]: _at_0}, ["output"]: _output_0}]);
  } else {
    const _reason_1 = _t_0["fault"];
    const _at_1 = _t_0["at"];
    const _output_1 = _vm_0["output"];
    return {$: "VM", ["mem"]: _mem_0, ["ds"]: _ds_0, ["cs"]: _cs_0, ["ip"]: _ip_0, ["control"]: {$: "Control", ["mode"]: _t_1, ["fault"]: _reason_1, ["at"]: _at_1}, ["output"]: _output_1};
  }
}

function $text$stack_go$(_xs_0, _acc_0) {
  if (_xs_0.$ === "Nil") {
    return {$: "Tuple", ["fst"]: {$: "Nil"}, ["snd"]: _acc_0};
  } else {
    const _h_0 = _xs_0["head"];
    const _t_0 = _xs_0["tail"];
    const _x_0 = run_loop($text$cell$(_h_0));
    const _x_1 = (" " + _acc_0);
    return run_jump($vm$pair$, [run_loop($text$stack_go$(_t_0, run_loop($Bool$pick$(run_loop($String$is_empty$(_acc_0)), run_loop($text$cell$(_h_0)), (_x_0 + _x_1))))), run_clo((_x_2) => {
    return run_clo((_x_3) => {
    return {$: "Tuple", ["fst"]: {$: "Con", ["head"]: _h_0, ["tail"]: _x_2}, ["snd"]: _x_3};
});
})]);
  }
}

function $query_address$(_addr_0, _vm_0) {
  if (_addr_0.$ === "None") {
    return {$: "Tuple", ["fst"]: _vm_0, ["snd"]: "unknown query\n"};
  } else {
    const _a_0 = _addr_0["value"];
    return run_jump($vm$state$, [_vm_0, run_clo((_x_0) => {
    return run_clo((_x_1) => {
    return run_clo((_x_2) => {
    return run_clo((_x_3) => {
    return run_clo((_x_4) => {
    return run_clo((_x_5) => {
    return run_jump($vm$pair$, [run_loop($dump$(16n, _x_0, _a_0)), run_clo((_x_6) => {
    return run_clo((_x_7) => {
    return {$: "Tuple", ["fst"]: {$: "VM", ["mem"]: _x_6, ["ds"]: _x_1, ["cs"]: _x_2, ["ip"]: _x_3, ["control"]: _x_4, ["output"]: _x_5}, ["snd"]: (_x_7 + "\n")};
});
})]);
});
});
});
});
});
})]);
  }
}

function $vm$read_cell$(_mem_0, _a_0) {
  return run_jump($vm$pair$, [{$: "Tuple", fst: _mem_0, snd: _mem_0[_a_0 % _mem_0.length]}, run_clo((_x_0) => {
  return run_clo((_x_1) => {
  const _x_2 = ((_a_0 + 1) >>> 0);
  return run_jump($vm$pair$, [{$: "Tuple", fst: _x_0, snd: _x_0[_x_2 % _x_0.length]}, run_clo((_x_3) => {
  return run_clo((_x_4) => {
  const _x_5 = ((_a_0 + 2) >>> 0);
  return run_jump($vm$pair$, [{$: "Tuple", fst: _x_3, snd: _x_3[_x_5 % _x_3.length]}, run_clo((_x_6) => {
  return run_clo((_x_7) => {
  const _x_8 = ((_a_0 + 3) >>> 0);
  return run_jump($vm$pair$, [{$: "Tuple", fst: _x_6, snd: _x_6[_x_8 % _x_6.length]}, run_clo((_x_9) => {
  return run_clo((_x_10) => {
  const _lo_0 = run_loop($vm$pack_half$(_x_1, _x_4));
  const _hi_0 = run_loop($vm$pack_half$(_x_7, _x_10));
  const _x_11 = (16n >= 32n ? 0 : (_hi_0 << Number(16n)) >>> 0);
  return {$: "Tuple", ["fst"]: _x_9, ["snd"]: ((_lo_0 | _x_11) >>> 0)};
});
})]);
});
})]);
});
})]);
});
})]);
}

function $text$hex_fixed$(_n_0, _x_0, _acc_0) {
  if (_n_0 === 0n) {
    return _acc_0;
  } else {
    const _p_0 = (_n_0 - 1n);
    return run_jump($text$hex_fixed$, [_p_0, (4n >= 32n ? 0 : (_x_0 >>> Number(4n)) >>> 0), (run_loop($text$digit$(((_x_0 & 15) >>> 0))) + _acc_0)]);
  }
}

function $fault_text$(_reason_0) {
  if (_reason_0.$ === "Clear") {
    return "clear";
  } else if (_reason_0.$ === "DataUnderflow") {
    return "data-underflow";
  } else if (_reason_0.$ === "ControlUnderflow") {
    return "control-underflow";
  } else if (_reason_0.$ === "Address") {
    return "invalid-address";
  } else if (_reason_0.$ === "ZeroDivisor") {
    return "division-by-zero";
  } else if (_reason_0.$ === "ShiftRange") {
    return "shift-out-of-range";
  } else if (_reason_0.$ === "Opcode") {
    return "unsupported-opcode";
  } else {
    return "unsupported-device";
  }
}

function $text$trim_zero$(_s_0) {
  if (_s_0 === "") {
    return "0";
  } else {
    const _t_0 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(0, 2) : _s_0[0]);
    const _t_1 = _t_0.codePointAt(0);
    if (_t_1 == 48) {
      const _t_2 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(2) : _s_0.slice(1));
      return run_jump($text$trim_zero$, [_t_2]);
    } else {
      const _13_0 = u32_to_word(_t_1)["head"];
      const _14_0 = u32_to_word(_t_1)["tail"];
      const _t_3 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(2) : _s_0.slice(1));
      return (char_new(word_to_u32({$: "WCon", ["head"]: _13_0, ["tail"]: _14_0})) + _t_3);
    }
  }
}

function $Map$set$(_m_0, _key_0, _x_0) {
  return run_jump($Map$set$go$, [_x_0, run_loop($Map$seek$(_m_0, _key_0))]);
}

function $set_here$(_vm_0, _addr_0) {
  const _mem_0 = _vm_0["mem"];
  const _ds_0 = _vm_0["ds"];
  const _cs_0 = _vm_0["cs"];
  const _ip_0 = _vm_0["ip"];
  const _control_0 = _vm_0["control"];
  const _output_0 = _vm_0["output"];
  return {$: "VM", ["mem"]: run_loop($vm$write_cell$(_mem_0, 124, _addr_0)), ["ds"]: _ds_0, ["cs"]: _cs_0, ["ip"]: _ip_0, ["control"]: _control_0, ["output"]: _output_0};
}

function $text$negative_hex$(_n_0) {
  if (_n_0.$ === "None") {
    return {$: "None"};
  } else {
    const _x_0 = _n_0["value"];
    return {$: "Some", ["value"]: run_loop($vm$negate$(_x_0))};
  }
}

function $text$read_hex_go$(_s_0, _x_0, _ok_0) {
  if (_s_0 === "") {
    return run_jump($text$parsed$, [_ok_0, _x_0]);
  } else {
    const _t_0 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(0, 2) : _s_0[0]);
    const _c_0 = _t_0.codePointAt(0);
    const _t_1 = (_s_0.codePointAt(0) > 0xFFFF ? _s_0.slice(2) : _s_0.slice(1));
    const _d_0 = run_loop($text$digit_value$(_c_0));
    const _x_1 = (Math.imul(_x_0, 16) >>> 0);
    return run_jump($text$read_hex_go$, [_t_1, ((_x_1 + _d_0) >>> 0), run_loop($Bool$and$(_ok_0, (_d_0 < 16)))]);
  }
}

function $text$find_name$(_xs_0, _name_0, _index_0) {
  if (_xs_0.$ === "Nil") {
    return 256;
  } else {
    const _h_0 = _xs_0["head"];
    const _t_0 = _xs_0["tail"];
    return run_jump($vm$pick$, [run_loop($String$eq$(_h_0, _name_0)), _index_0, run_loop($text$find_name$(_t_0, _name_0, ((_index_0 + 1) >>> 0)))]);
  }
}

function $text$core_names$() {
  return run_jump($String$split$, ["ad sb ml dv md sh an or xr nt eq lt du sw ov zp dc cd rb ri wb wi lb li rs ls jm hp h0 cl rt nx", " "]);
}

function $literal$(_n_0, _name_0, _vm_0, _labels_0, _assembling_0, _quit_0, _report_0) {
  if (_n_0.$ === "Some") {
    const _x_0 = _n_0["value"];
    return {$: "Shell", ["vm"]: run_loop($number$(_assembling_0, _vm_0, _x_0)), ["labels"]: _labels_0, ["assembling"]: _assembling_0, ["quit"]: _quit_0, ["report"]: _report_0};
  } else {
    return run_jump($vm$pair$, [run_loop($Map$get$(0, _labels_0, _name_0)), run_clo((_x_1) => {
    return run_clo((_x_2) => {
    return run_jump($found_label$, [(_x_2 !== 0), _x_2, _name_0, _vm_0, _x_1, _assembling_0, _quit_0, _report_0]);
});
})]);
  }
}

function $calculator$(_vm_0, _op_0) {
  const _mem_0 = _vm_0["mem"];
  const _ds_0 = _vm_0["ds"];
  const _cs_0 = _vm_0["cs"];
  const _ip_0 = _vm_0["ip"];
  const _t_0 = _vm_0["control"];
  const _t_1 = _t_0["mode"];
  if (_t_1.$ === "Ti") {
    const _reason_0 = _t_0["fault"];
    const _at_0 = _t_0["at"];
    const _output_0 = _vm_0["output"];
    return run_jump($vm$execute$, [_op_0, {$: "VM", ["mem"]: _mem_0, ["ds"]: _ds_0, ["cs"]: _cs_0, ["ip"]: _ip_0, ["control"]: {$: "Control", ["mode"]: {$: "Ti"}, ["fault"]: _reason_0, ["at"]: _at_0}, ["output"]: _output_0}]);
  } else {
    const _reason_1 = _t_0["fault"];
    const _at_1 = _t_0["at"];
    const _output_1 = _vm_0["output"];
    return {$: "VM", ["mem"]: _mem_0, ["ds"]: _ds_0, ["cs"]: _cs_0, ["ip"]: _ip_0, ["control"]: {$: "Control", ["mode"]: _t_1, ["fault"]: _reason_1, ["at"]: _at_1}, ["output"]: _output_1};
  }
}

function $invoke_register$(_vm_0, _r_0) {
  return run_jump($vm$state$, [_vm_0, run_clo((_x_0) => {
  return run_clo((_x_1) => {
  return run_clo((_x_2) => {
  return run_clo((_x_3) => {
  return run_clo((_x_4) => {
  return run_clo((_x_5) => {
  return run_jump($vm$pair$, [run_loop($vm$read_cell$(_x_0, (Math.imul(_r_0, 4) >>> 0))), run_clo((_x_6) => {
  return run_clo((_x_7) => {
  return run_jump($invoke$, [{$: "VM", ["mem"]: _x_6, ["ds"]: _x_1, ["cs"]: _x_2, ["ip"]: _x_3, ["control"]: _x_4, ["output"]: _x_5}, _x_7]);
});
})]);
});
});
});
});
});
})]);
}

function $vm$fault$(_vm_0, _reason_0) {
  const _mem_0 = _vm_0["mem"];
  const _ds_0 = _vm_0["ds"];
  const _cs_0 = _vm_0["cs"];
  const _ip_0 = _vm_0["ip"];
  const _control_0 = _vm_0["control"];
  const _output_0 = _vm_0["output"];
  return {$: "VM", ["mem"]: _mem_0, ["ds"]: _ds_0, ["cs"]: _cs_0, ["ip"]: _ip_0, ["control"]: {$: "Control", ["mode"]: {$: "Na"}, ["fault"]: _reason_0, ["at"]: _ip_0}, ["output"]: _output_0};
}

function $vm$advance$(_vm_0) {
  const _mem_0 = _vm_0["mem"];
  const _ds_0 = _vm_0["ds"];
  const _cs_0 = _vm_0["cs"];
  const _ip_0 = _vm_0["ip"];
  const _t_0 = _vm_0["control"];
  const _t_1 = _t_0["mode"];
  if (_t_1.$ === "Na") {
    const _reason_0 = _t_0["fault"];
    const _at_0 = _t_0["at"];
    const _output_0 = _vm_0["output"];
    return {$: "VM", ["mem"]: _mem_0, ["ds"]: _ds_0, ["cs"]: _cs_0, ["ip"]: _ip_0, ["control"]: {$: "Control", ["mode"]: {$: "Na"}, ["fault"]: _reason_0, ["at"]: _at_0}, ["output"]: _output_0};
  } else {
    const _reason_1 = _t_0["fault"];
    const _at_1 = _t_0["at"];
    const _output_1 = _vm_0["output"];
    return {$: "VM", ["mem"]: _mem_0, ["ds"]: _ds_0, ["cs"]: _cs_0, ["ip"]: ((_ip_0 + 1) >>> 0), ["control"]: {$: "Control", ["mode"]: _t_1, ["fault"]: _reason_1, ["at"]: _at_1}, ["output"]: _output_1};
  }
}

function $vm$execute$(_op_0, _vm_0) {
  const _mem_0 = _vm_0["mem"];
  const _ds_0 = _vm_0["ds"];
  const _cs_0 = _vm_0["cs"];
  const _ip_0 = _vm_0["ip"];
  const _control_0 = _vm_0["control"];
  const _output_0 = _vm_0["output"];
  return run_jump($vm$validated$, [run_loop($vm$stack_check$(run_loop($vm$supported$(_op_0)), run_loop($vm$enough$(run_loop($vm$need_data$(_op_0)), _ds_0)), run_loop($vm$enough$(run_loop($vm$need_control$(_op_0)), _cs_0)))), _op_0, {$: "VM", ["mem"]: _mem_0, ["ds"]: _ds_0, ["cs"]: _cs_0, ["ip"]: _ip_0, ["control"]: _control_0, ["output"]: _output_0}]);
}

function $text$cell$(_x_0) {
  return run_jump($text$signed$, [run_loop($vm$negative$(_x_0)), _x_0]);
}

function $dump$(_n_0, _mem_0, _addr_0) {
  if (_n_0 === 0n) {
    return {$: "Tuple", ["fst"]: _mem_0, ["snd"]: ""};
  } else {
    const _p_0 = (_n_0 - 1n);
    return run_jump($vm$pair$, [{$: "Tuple", fst: _mem_0, snd: _mem_0[_addr_0 % _mem_0.length]}, run_clo((_x_0) => {
    return run_clo((_x_1) => {
    return run_jump($vm$pair$, [run_loop($dump$(_p_0, _x_0, ((_addr_0 + 1) >>> 0))), run_clo((_x_2) => {
    return run_clo((_x_3) => {
    const _x_4 = run_loop($text$byte$(_x_1));
    const _x_5 = run_loop($Bool$pick$(run_loop($String$is_empty$(_x_3)), "", (" " + _x_3)));
    return {$: "Tuple", ["fst"]: _x_2, ["snd"]: (_x_4 + _x_5)};
});
})]);
});
})]);
  }
}

function $vm$pack_half$(_a_0, _b_0) {
  const _x_0 = (8n >= 32n ? 0 : (_b_0 << Number(8n)) >>> 0);
  return ((_a_0 | _x_0) >>> 0);
}

function $text$digit$(_n_0) {
  return char_new(run_loop($vm$pick$((_n_0 < 10), ((_n_0 + 48) >>> 0), ((_n_0 + 55) >>> 0))));
}

function $Map$set$go$(_x_0, _r_0) {
  const _m2_0 = _r_0["fst"];
  const _t_0 = _r_0["snd"];
  const _key2_0 = _t_0["fst"];
  const _t_1 = _t_0["snd"];
  if (_t_1.$ === "None") {
    return {$: "MLeaf", ["key"]: _key2_0, ["val"]: _x_0};
  } else {
    const _k_0 = _t_1["value"];
    const _ka_0 = _key2_0;
    return run_jump($Map$set$fin$, [_m2_0, _ka_0, _x_0, _ka_0, _k_0]);
  }
}

function $Map$seek$(_m_0, _key_0) {
  if (_m_0.$ === "MTip") {
    return {$: "Tuple", ["fst"]: {$: "MTip"}, ["snd"]: {$: "Tuple", ["fst"]: _key_0, ["snd"]: {$: "None"}}};
  } else if (_m_0.$ === "MLeaf") {
    const _k_0 = _m_0["key"];
    const _v_0 = _m_0["val"];
    return {$: "Tuple", ["fst"]: {$: "MLeaf", ["key"]: _k_0, ["val"]: _v_0}, ["snd"]: {$: "Tuple", ["fst"]: _key_0, ["snd"]: {$: "Some", ["value"]: _k_0}}};
  } else {
    const _pos_0 = _m_0["pos"];
    const _lo_0 = _m_0["lo"];
    const _hi_0 = _m_0["hi"];
    return run_jump($Map$seek$bit$, [_lo_0, _hi_0, _pos_0, run_loop($Map$bit$(_key_0, _pos_0))]);
  }
}

function $vm$negate$(_x_0) {
  return ((0 - _x_0) >>> 0);
}

function $text$digit_value$(_c_0) {
  return run_jump($vm$pick$, [run_loop($Bool$and$((_c_0 >= 48), (_c_0 <= 57))), ((_c_0 - 48) >>> 0), run_loop($vm$pick$(run_loop($Bool$and$((_c_0 >= 65), (_c_0 <= 70))), ((_c_0 - 55) >>> 0), run_loop($vm$pick$(run_loop($Bool$and$((_c_0 >= 97), (_c_0 <= 102))), ((_c_0 - 87) >>> 0), 16))))]);
}

function $vm$pick$(_c_0, _a_0, _b_0) {
  return run_jump($Bool$pick$, [_c_0, _a_0, _b_0]);
}

function $String$eq$(_a_0, _b_0) {
  return run_jump($String$eq$fin$, [run_loop($String$cmp$(_a_0, _b_0))]);
}

function $Map$get$(_d_0, _m_0, _key_0) {
  if (_m_0.$ === "MTip") {
    return {$: "Tuple", ["fst"]: {$: "MTip"}, ["snd"]: _d_0};
  } else if (_m_0.$ === "MLeaf") {
    const _k_0 = _m_0["key"];
    const _v_0 = _m_0["val"];
    return run_jump($Map$get$leaf$, [_d_0, _v_0, run_loop($String$cmp$(_key_0, _k_0))]);
  } else {
    const _pos_0 = _m_0["pos"];
    const _lo_0 = _m_0["lo"];
    const _hi_0 = _m_0["hi"];
    return run_jump($Map$get$bit$, [_d_0, _lo_0, _hi_0, _pos_0, run_loop($Map$bit$(_key_0, _pos_0))]);
  }
}

function $found_label$(_found_0, _addr_0, _name_0, _vm_0, _labels_0, _assembling_0, _quit_0, _report_0) {
  if (_found_0) {
    return {$: "Shell", ["vm"]: run_loop($label_call$(_assembling_0, _vm_0, _addr_0)), ["labels"]: _labels_0, ["assembling"]: _assembling_0, ["quit"]: _quit_0, ["report"]: _report_0};
  } else {
    const _x_0 = (_name_0 + "\n");
    const _x_1 = ("unknown token: " + _x_0);
    return {$: "Shell", ["vm"]: _vm_0, ["labels"]: _labels_0, ["assembling"]: _assembling_0, ["quit"]: _quit_0, ["report"]: (_report_0 + _x_1)};
  }
}

function $invoke$(_vm_0, _addr_0) {
  return run_jump($invoke_checked$, [run_loop($vm$code_ok$(_addr_0)), _vm_0, _addr_0]);
}

function $vm$validated$(_reason_0, _op_0, _vm_0) {
  if (_reason_0.$ === "Clear") {
    const _mem_0 = _vm_0["mem"];
    const _ds_0 = _vm_0["ds"];
    const _cs_0 = _vm_0["cs"];
    const _ip_0 = _vm_0["ip"];
    const _control_0 = _vm_0["control"];
    const _output_0 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$operands$(_op_0, _mem_0, _ds_0, _cs_0, _ip_0)), run_clo((_x_0) => {
    return run_clo((_x_1) => {
    return run_jump($vm$commit$, [_x_1, _op_0, {$: "VM", ["mem"]: _x_0, ["ds"]: _ds_0, ["cs"]: _cs_0, ["ip"]: _ip_0, ["control"]: _control_0, ["output"]: _output_0}]);
});
})]);
  } else {
    return run_jump($vm$fault$, [_vm_0, _reason_0]);
  }
}

function $vm$stack_check$(_known_0, _data_0, _ctrl_0) {
  if (!_known_0) {
    return {$: "Opcode"};
  } else {
    if (!_data_0) {
      return {$: "DataUnderflow"};
    } else {
      if (!_ctrl_0) {
        return {$: "ControlUnderflow"};
      } else {
        return {$: "Clear"};
      }
    }
  }
}

function $vm$supported$(_op_0) {
  const _x_0 = run_loop($Bool$and$((_op_0 >= 246), (_op_0 <= 248)));
  const _x_1 = run_loop($Bool$and$((_op_0 >= 253), (_op_0 <= 255)));
  const _x_2 = (_op_0 === 193);
  const _x_3 = (_x_0 || _x_1);
  const _x_4 = (_op_0 === 192);
  const _x_5 = (_x_2 || _x_3);
  const _x_6 = (_op_0 < 160);
  const _x_7 = (_x_4 || _x_5);
  return (_x_6 || _x_7);
}

function $vm$enough$(_n_0, _xs_0) {
  if (_n_0 === 0n) {
    return true;
  } else {
    const _p_0 = (_n_0 - 1n);
    if (_xs_0.$ === "Nil") {
      return false;
    } else {
      const _h_0 = _xs_0["head"];
      const _t_0 = _xs_0["tail"];
      return run_jump($vm$enough$, [_p_0, _t_0]);
    }
  }
}

function $vm$need_data$(_op_0) {
  if (_op_0 == 128) {
    return 2n;
  } else if ((_op_0 & 31) == 0) {
    const _17_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _18_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_0 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _17_0, ["tail"]: _18_0}}}}}});
    const _x_1 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _17_0, ["tail"]: _18_0}}}}}});
    return run_jump($Bool$pick$, [run_loop($Bool$and$((_x_0 >= 64), (_x_1 < 128))), 1n, 0n]);
  } else if (_op_0 == 144) {
    return 1n;
  } else if ((_op_0 & 31) == 16) {
    const _71_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _72_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_2 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _71_0, ["tail"]: _72_0}}}}}});
    const _x_3 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _71_0, ["tail"]: _72_0}}}}}});
    return run_jump($Bool$pick$, [run_loop($Bool$and$((_x_2 >= 64), (_x_3 < 128))), 1n, 0n]);
  } else if (_op_0 == 136) {
    return 2n;
  } else if ((_op_0 & 31) == 8) {
    const _127_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _128_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_4 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _127_0, ["tail"]: _128_0}}}}}});
    const _x_5 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _127_0, ["tail"]: _128_0}}}}}});
    return run_jump($Bool$pick$, [run_loop($Bool$and$((_x_4 >= 64), (_x_5 < 128))), 1n, 0n]);
  } else if (_op_0 == 152) {
    return 1n;
  } else if ((_op_0 & 31) == 24) {
    const _181_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _182_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_6 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _181_0, ["tail"]: _182_0}}}}}});
    const _x_7 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _181_0, ["tail"]: _182_0}}}}}});
    return run_jump($Bool$pick$, [run_loop($Bool$and$((_x_6 >= 64), (_x_7 < 128))), 1n, 0n]);
  } else if (_op_0 == 132) {
    return 2n;
  } else if ((_op_0 & 31) == 4) {
    const _239_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _240_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_8 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _239_0, ["tail"]: _240_0}}}}}});
    const _x_9 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _239_0, ["tail"]: _240_0}}}}}});
    return run_jump($Bool$pick$, [run_loop($Bool$and$((_x_8 >= 64), (_x_9 < 128))), 1n, 0n]);
  } else if (_op_0 == 148) {
    return 2n;
  } else if ((_op_0 & 31) == 20) {
    const _293_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _294_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_10 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _293_0, ["tail"]: _294_0}}}}}});
    const _x_11 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _293_0, ["tail"]: _294_0}}}}}});
    return run_jump($Bool$pick$, [run_loop($Bool$and$((_x_10 >= 64), (_x_11 < 128))), 1n, 0n]);
  } else if (_op_0 == 140) {
    return 1n;
  } else if ((_op_0 & 31) == 12) {
    const _349_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _350_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_12 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _349_0, ["tail"]: _350_0}}}}}});
    const _x_13 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _349_0, ["tail"]: _350_0}}}}}});
    return run_jump($Bool$pick$, [run_loop($Bool$and$((_x_12 >= 64), (_x_13 < 128))), 1n, 0n]);
  } else if (_op_0 == 156) {
    return 1n;
  } else if ((_op_0 & 31) == 28) {
    const _403_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _404_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_14 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _403_0, ["tail"]: _404_0}}}}}});
    const _x_15 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _403_0, ["tail"]: _404_0}}}}}});
    return run_jump($Bool$pick$, [run_loop($Bool$and$((_x_14 >= 64), (_x_15 < 128))), 1n, 0n]);
  } else if (_op_0 == 130) {
    return 2n;
  } else if ((_op_0 & 31) == 2) {
    const _463_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _464_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_16 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _463_0, ["tail"]: _464_0}}}}}});
    const _x_17 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _463_0, ["tail"]: _464_0}}}}}});
    return run_jump($Bool$pick$, [run_loop($Bool$and$((_x_16 >= 64), (_x_17 < 128))), 1n, 0n]);
  } else if (_op_0 == 146) {
    return 1n;
  } else if ((_op_0 & 31) == 18) {
    const _517_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _518_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_18 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _517_0, ["tail"]: _518_0}}}}}});
    const _x_19 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _517_0, ["tail"]: _518_0}}}}}});
    return run_jump($Bool$pick$, [run_loop($Bool$and$((_x_18 >= 64), (_x_19 < 128))), 1n, 0n]);
  } else if (_op_0 == 138) {
    return 2n;
  } else if ((_op_0 & 15) == 10) {
    const _571_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["head"];
    const _572_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_20 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _571_0, ["tail"]: _572_0}}}}});
    const _x_21 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _571_0, ["tail"]: _572_0}}}}});
    return run_jump($Bool$pick$, [run_loop($Bool$and$((_x_20 >= 64), (_x_21 < 128))), 1n, 0n]);
  } else if (_op_0 == 134) {
    return 2n;
  } else if ((_op_0 & 15) == 6) {
    const _629_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["head"];
    const _630_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_22 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _629_0, ["tail"]: _630_0}}}}});
    const _x_23 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _629_0, ["tail"]: _630_0}}}}});
    return run_jump($Bool$pick$, [run_loop($Bool$and$((_x_22 >= 64), (_x_23 < 128))), 1n, 0n]);
  } else if (_op_0 == 142) {
    return 2n;
  } else if ((_op_0 & 15) == 14) {
    const _685_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["head"];
    const _686_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_24 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _685_0, ["tail"]: _686_0}}}}});
    const _x_25 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _685_0, ["tail"]: _686_0}}}}});
    return run_jump($Bool$pick$, [run_loop($Bool$and$((_x_24 >= 64), (_x_25 < 128))), 1n, 0n]);
  } else if (_op_0 == 129) {
    return 2n;
  } else if ((_op_0 & 15) == 1) {
    const _747_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["head"];
    const _748_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_26 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _747_0, ["tail"]: _748_0}}}}});
    const _x_27 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _747_0, ["tail"]: _748_0}}}}});
    return run_jump($Bool$pick$, [run_loop($Bool$and$((_x_26 >= 64), (_x_27 < 128))), 1n, 0n]);
  } else if (_op_0 == 137) {
    return 1n;
  } else if ((_op_0 & 15) == 9) {
    const _803_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["head"];
    const _804_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_28 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _803_0, ["tail"]: _804_0}}}}});
    const _x_29 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _803_0, ["tail"]: _804_0}}}}});
    return run_jump($Bool$pick$, [run_loop($Bool$and$((_x_28 >= 64), (_x_29 < 128))), 1n, 0n]);
  } else if (_op_0 == 133) {
    return 2n;
  } else if ((_op_0 & 31) == 5) {
    const _863_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _864_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_30 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _863_0, ["tail"]: _864_0}}}}}});
    const _x_31 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _863_0, ["tail"]: _864_0}}}}}});
    return run_jump($Bool$pick$, [run_loop($Bool$and$((_x_30 >= 64), (_x_31 < 128))), 1n, 0n]);
  } else if (_op_0 == 149) {
    return 2n;
  } else if ((_op_0 & 31) == 21) {
    const _917_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _918_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_32 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _917_0, ["tail"]: _918_0}}}}}});
    const _x_33 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _917_0, ["tail"]: _918_0}}}}}});
    return run_jump($Bool$pick$, [run_loop($Bool$and$((_x_32 >= 64), (_x_33 < 128))), 1n, 0n]);
  } else if (_op_0 == 141) {
    return 2n;
  } else if ((_op_0 & 31) == 13) {
    const _973_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _974_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_34 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _973_0, ["tail"]: _974_0}}}}}});
    const _x_35 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _973_0, ["tail"]: _974_0}}}}}});
    return run_jump($Bool$pick$, [run_loop($Bool$and$((_x_34 >= 64), (_x_35 < 128))), 1n, 0n]);
  } else if (_op_0 == 253) {
    return 2n;
  } else if ((_op_0 & 31) == 29) {
    const _1027_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _1028_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_36 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _1027_0, ["tail"]: _1028_0}}}}}});
    const _x_37 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _1027_0, ["tail"]: _1028_0}}}}}});
    return run_jump($Bool$pick$, [run_loop($Bool$and$((_x_36 >= 64), (_x_37 < 128))), 1n, 0n]);
  } else if (_op_0 == 131) {
    return 2n;
  } else if ((_op_0 & 31) == 3) {
    const _1087_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _1088_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_38 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _1087_0, ["tail"]: _1088_0}}}}}});
    const _x_39 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _1087_0, ["tail"]: _1088_0}}}}}});
    return run_jump($Bool$pick$, [run_loop($Bool$and$((_x_38 >= 64), (_x_39 < 128))), 1n, 0n]);
  } else if (_op_0 == 147) {
    return 1n;
  } else if ((_op_0 & 31) == 19) {
    const _1141_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _1142_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_40 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _1141_0, ["tail"]: _1142_0}}}}}});
    const _x_41 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _1141_0, ["tail"]: _1142_0}}}}}});
    return run_jump($Bool$pick$, [run_loop($Bool$and$((_x_40 >= 64), (_x_41 < 128))), 1n, 0n]);
  } else if (_op_0 == 139) {
    return 2n;
  } else if ((_op_0 & 15) == 11) {
    const _1195_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["head"];
    const _1196_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_42 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _1195_0, ["tail"]: _1196_0}}}}});
    const _x_43 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _1195_0, ["tail"]: _1196_0}}}}});
    return run_jump($Bool$pick$, [run_loop($Bool$and$((_x_42 >= 64), (_x_43 < 128))), 1n, 0n]);
  } else if (_op_0 == 135) {
    return 2n;
  } else if ((_op_0 & 15) == 7) {
    const _1253_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["head"];
    const _1254_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_44 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _1253_0, ["tail"]: _1254_0}}}}});
    const _x_45 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _1253_0, ["tail"]: _1254_0}}}}});
    return run_jump($Bool$pick$, [run_loop($Bool$and$((_x_44 >= 64), (_x_45 < 128))), 1n, 0n]);
  } else if (_op_0 == 143) {
    return 1n;
  } else {
    const _1309_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["head"];
    const _1310_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_46 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _1309_0, ["tail"]: _1310_0}}}}});
    const _x_47 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _1309_0, ["tail"]: _1310_0}}}}});
    return run_jump($Bool$pick$, [run_loop($Bool$and$((_x_46 >= 64), (_x_47 < 128))), 1n, 0n]);
  }
}

function $vm$need_control$(_op_0) {
  if (_op_0 == 145) {
    return 1n;
  } else if ((_op_0 & 3) == 1) {
    const _10_0 = u32_to_word(_op_0)["tail"]["tail"]["head"];
    const _11_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"];
    return 0n;
  } else if (_op_0 == 159) {
    return 1n;
  } else if ((_op_0 & 3) == 3) {
    const _70_0 = u32_to_word(_op_0)["tail"]["tail"]["head"];
    const _71_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"];
    return 0n;
  } else if (_op_0 == 158) {
    return 1n;
  } else {
    const _130_0 = u32_to_word(_op_0)["tail"]["head"];
    const _131_0 = u32_to_word(_op_0)["tail"]["tail"];
    return 0n;
  }
}

function $text$signed$(_neg_0, _x_0) {
  if (_neg_0) {
    const _x_1 = run_loop($text$hex$(run_loop($vm$negate$(_x_0))));
    return ("-" + _x_1);
  } else {
    return run_jump($text$hex$, [_x_0]);
  }
}

function $vm$negative$(_x_0) {
  return (_x_0 >= 2147483648);
}

function $text$byte$(_x_0) {
  if (_x_0 == 0) {
    return "..";
  } else if (_x_0 == 192) {
    return "c0";
  } else if (_x_0 == 248) {
    return "c4";
  } else if ((_x_0 & 3) == 0) {
    const _11_0 = u32_to_word(_x_0)["tail"]["tail"]["head"];
    const _12_0 = u32_to_word(_x_0)["tail"]["tail"]["tail"];
    const _x_1 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _11_0, ["tail"]: _12_0}}});
    return run_jump($text$byte_other$, [(_x_1 < 128), word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _11_0, ["tail"]: _12_0}}})]);
  } else if (_x_0 == 246) {
    return "c2";
  } else if (_x_0 == 254) {
    return "db";
  } else if ((_x_0 & 3) == 2) {
    const _177_0 = u32_to_word(_x_0)["tail"]["tail"]["head"];
    const _178_0 = u32_to_word(_x_0)["tail"]["tail"]["tail"];
    const _x_2 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _177_0, ["tail"]: _178_0}}});
    return run_jump($text$byte_other$, [(_x_2 < 128), word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _177_0, ["tail"]: _178_0}}})]);
  } else if (_x_0 == 193) {
    return "c1";
  } else if ((_x_0 & 7) == 1) {
    const _297_0 = u32_to_word(_x_0)["tail"]["tail"]["tail"]["head"];
    const _298_0 = u32_to_word(_x_0)["tail"]["tail"]["tail"]["tail"];
    const _x_3 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _297_0, ["tail"]: _298_0}}}});
    return run_jump($text$byte_other$, [(_x_3 < 128), word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _297_0, ["tail"]: _298_0}}}})]);
  } else if (_x_0 == 253) {
    return "io";
  } else if ((_x_0 & 7) == 5) {
    const _355_0 = u32_to_word(_x_0)["tail"]["tail"]["tail"]["head"];
    const _356_0 = u32_to_word(_x_0)["tail"]["tail"]["tail"]["tail"];
    const _x_4 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _355_0, ["tail"]: _356_0}}}});
    return run_jump($text$byte_other$, [(_x_4 < 128), word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _355_0, ["tail"]: _356_0}}}})]);
  } else if (_x_0 == 247) {
    return "n1";
  } else if (_x_0 == 255) {
    return "hl";
  } else {
    const _413_0 = u32_to_word(_x_0)["tail"]["tail"]["head"];
    const _414_0 = u32_to_word(_x_0)["tail"]["tail"]["tail"];
    const _x_5 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _413_0, ["tail"]: _414_0}}});
    return run_jump($text$byte_other$, [(_x_5 < 128), word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _413_0, ["tail"]: _414_0}}})]);
  }
}

function $Map$set$fin$(_m_0, _key_0, _x_0, _keyb_0, _k_0) {
  return run_jump($Map$set$fin$go$, [_m_0, _key_0, _x_0, run_loop($String$cmp$(_keyb_0, _k_0))]);
}

function $Map$seek$bit$(_lo_0, _hi_0, _p2_0, _kb_0) {
  const _key2_0 = _kb_0["fst"];
  const _t_0 = _kb_0["snd"];
  if (!_t_0) {
    return run_jump($Map$lo$, [_p2_0, _hi_0, run_loop($Map$seek$(_lo_0, _key2_0))]);
  } else {
    return run_jump($Map$hi$, [_p2_0, _lo_0, run_loop($Map$seek$(_hi_0, _key2_0))]);
  }
}

function $Map$bit$(_key_0, _pos_0) {
  return run_jump($Map$bit$at$, [_key_0, nat_divmod(_pos_0, 33n)]);
}

function $String$eq$fin$(_r_0) {
  const _t_0 = _r_0["fst"];
  const _a2_0 = _t_0["fst"];
  const _b2_0 = _t_0["snd"];
  const _c_0 = _r_0["snd"];
  return run_jump($Cmp$is_eq$, [_c_0]);
}

function $String$cmp$(_a_0, _b_0) {
  if (_a_0 === "") {
    if (_b_0 === "") {
      return {$: "Tuple", ["fst"]: {$: "Tuple", ["fst"]: "", ["snd"]: ""}, ["snd"]: {$: "EQ"}};
    } else {
      const _h_0 = (_b_0.codePointAt(0) > 0xFFFF ? _b_0.slice(0, 2) : _b_0[0]);
      const _t_0 = (_b_0.codePointAt(0) > 0xFFFF ? _b_0.slice(2) : _b_0.slice(1));
      return {$: "Tuple", ["fst"]: {$: "Tuple", ["fst"]: "", ["snd"]: (_h_0 + _t_0)}, ["snd"]: {$: "LT"}};
    }
  } else {
    const _h_1 = (_a_0.codePointAt(0) > 0xFFFF ? _a_0.slice(0, 2) : _a_0[0]);
    const _t_1 = (_a_0.codePointAt(0) > 0xFFFF ? _a_0.slice(2) : _a_0.slice(1));
    if (_b_0 === "") {
      return {$: "Tuple", ["fst"]: {$: "Tuple", ["fst"]: (_h_1 + _t_1), ["snd"]: ""}, ["snd"]: {$: "GT"}};
    } else {
      const _h2_0 = (_b_0.codePointAt(0) > 0xFFFF ? _b_0.slice(0, 2) : _b_0[0]);
      const _t2_0 = (_b_0.codePointAt(0) > 0xFFFF ? _b_0.slice(2) : _b_0.slice(1));
      return run_jump($String$cmp$fin$, [_t_1, _t2_0, run_loop($Char$cmp$(_h_1, _h2_0))]);
    }
  }
}

function $Map$get$leaf$(_d_0, _v_0, _r_0) {
  const _t_0 = _r_0["fst"];
  const _key2_0 = _t_0["fst"];
  const _k2_0 = _t_0["snd"];
  const _t_1 = _r_0["snd"];
  if (_t_1.$ === "LT") {
    return {$: "Tuple", ["fst"]: {$: "MLeaf", ["key"]: _k2_0, ["val"]: _v_0}, ["snd"]: _d_0};
  } else if (_t_1.$ === "EQ") {
    return {$: "Tuple", ["fst"]: {$: "MLeaf", ["key"]: _k2_0, ["val"]: _v_0}, ["snd"]: _v_0};
  } else {
    return {$: "Tuple", ["fst"]: {$: "MLeaf", ["key"]: _k2_0, ["val"]: _v_0}, ["snd"]: _d_0};
  }
}

function $Map$get$bit$(_d_0, _lo_0, _hi_0, _p2_0, _kb_0) {
  const _key2_0 = _kb_0["fst"];
  const _t_0 = _kb_0["snd"];
  if (!_t_0) {
    return run_jump($Map$lo$, [_p2_0, _hi_0, run_loop($Map$get$(_d_0, _lo_0, _key2_0))]);
  } else {
    return run_jump($Map$hi$, [_p2_0, _lo_0, run_loop($Map$get$(_d_0, _hi_0, _key2_0))]);
  }
}

function $label_call$(_assembling_0, _vm_0, _addr_0) {
  if (_assembling_0) {
    return run_jump($emit_call$, [_vm_0, _addr_0]);
  } else {
    return run_jump($invoke$, [_vm_0, _addr_0]);
  }
}

function $invoke_checked$(_ok_0, _vm_0, _addr_0) {
  if (_ok_0) {
    return run_jump($invoke_valid$, [_vm_0, _addr_0]);
  } else {
    return run_jump($vm$fault$, [_vm_0, {$: "Address"}]);
  }
}

function $vm$operands$(_op_0, _mem_0, _ds_0, _cs_0, _ip_0) {
  if (_op_0 == 131) {
    const _x_0 = run_loop($vm$top$(_ds_0));
    return {$: "Tuple", ["fst"]: _mem_0, ["snd"]: run_loop($vm$check$((_x_0 !== 0), {$: "ZeroDivisor"}))};
  } else if ((_op_0 & 31) == 3) {
    const _70_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _71_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_1 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _70_0, ["tail"]: _71_0}}}}}});
    const _x_2 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _70_0, ["tail"]: _71_0}}}}}});
    return run_jump($vm$register_target$, [run_loop($Bool$and$((_x_1 > 0), (_x_2 < 32))), _mem_0, word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _70_0, ["tail"]: _71_0}}}}}})]);
  } else if (_op_0 == 147) {
    return {$: "Tuple", ["fst"]: _mem_0, ["snd"]: run_loop($vm$check$(run_loop($vm$address_ok$(run_loop($vm$top$(_ds_0)), 4)), {$: "Address"}))};
  } else if ((_op_0 & 31) == 19) {
    const _124_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _125_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_3 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _124_0, ["tail"]: _125_0}}}}}});
    const _x_4 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _124_0, ["tail"]: _125_0}}}}}});
    return run_jump($vm$register_target$, [run_loop($Bool$and$((_x_3 > 0), (_x_4 < 32))), _mem_0, word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _124_0, ["tail"]: _125_0}}}}}})]);
  } else if (_op_0 == 155) {
    return run_jump($vm$check_hop$, [(_ip_0 < 65535), _mem_0, _ip_0]);
  } else if ((_op_0 & 15) == 11) {
    const _178_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["head"];
    const _179_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_5 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _178_0, ["tail"]: _179_0}}}}});
    const _x_6 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _178_0, ["tail"]: _179_0}}}}});
    return run_jump($vm$register_target$, [run_loop($Bool$and$((_x_5 > 0), (_x_6 < 32))), _mem_0, word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _178_0, ["tail"]: _179_0}}}}})]);
  } else if (_op_0 == 151) {
    return {$: "Tuple", ["fst"]: _mem_0, ["snd"]: run_loop($vm$check$((_ip_0 < 65532), {$: "Address"}))};
  } else if ((_op_0 & 15) == 7) {
    const _236_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["head"];
    const _237_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_7 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _236_0, ["tail"]: _237_0}}}}});
    const _x_8 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _236_0, ["tail"]: _237_0}}}}});
    return run_jump($vm$register_target$, [run_loop($Bool$and$((_x_7 > 0), (_x_8 < 32))), _mem_0, word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _236_0, ["tail"]: _237_0}}}}})]);
  } else if (_op_0 == 159) {
    const _x_9 = run_loop($vm$next_counter$(run_loop($vm$top$(_cs_0))));
    return run_jump($vm$branch_check$, [(_x_9 !== 0), _mem_0, _ip_0]);
  } else if ((_op_0 & 15) == 15) {
    const _292_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["head"];
    const _293_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_10 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _292_0, ["tail"]: _293_0}}}}});
    const _x_11 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _292_0, ["tail"]: _293_0}}}}});
    return run_jump($vm$register_target$, [run_loop($Bool$and$((_x_10 > 0), (_x_11 < 32))), _mem_0, word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _292_0, ["tail"]: _293_0}}}}})]);
  } else if (_op_0 == 133) {
    const _x_12 = run_loop($vm$magnitude$(run_loop($vm$top$(_ds_0))));
    return {$: "Tuple", ["fst"]: _mem_0, ["snd"]: run_loop($vm$check$((_x_12 < 32), {$: "ShiftRange"}))};
  } else if ((_op_0 & 31) == 5) {
    const _354_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _355_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_13 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _354_0, ["tail"]: _355_0}}}}}});
    const _x_14 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _354_0, ["tail"]: _355_0}}}}}});
    return run_jump($vm$register_target$, [run_loop($Bool$and$((_x_13 > 0), (_x_14 < 32))), _mem_0, word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _354_0, ["tail"]: _355_0}}}}}})]);
  } else if (_op_0 == 149) {
    return {$: "Tuple", ["fst"]: _mem_0, ["snd"]: run_loop($vm$check$(run_loop($vm$address_ok$(run_loop($vm$top$(_ds_0)), 4)), {$: "Address"}))};
  } else if ((_op_0 & 31) == 21) {
    const _408_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _409_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_15 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _408_0, ["tail"]: _409_0}}}}}});
    const _x_16 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _408_0, ["tail"]: _409_0}}}}}});
    return run_jump($vm$register_target$, [run_loop($Bool$and$((_x_15 > 0), (_x_16 < 32))), _mem_0, word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _408_0, ["tail"]: _409_0}}}}}})]);
  } else if (_op_0 == 157) {
    return run_jump($vm$check_target$, [(_ip_0 < 65532), _mem_0, ((_ip_0 + 1) >>> 0)]);
  } else if (_op_0 == 253) {
    const _x_17 = run_loop($vm$top$(_ds_0));
    return {$: "Tuple", ["fst"]: _mem_0, ["snd"]: run_loop($vm$check$((_x_17 === 101), {$: "Device"}))};
  } else if ((_op_0 & 15) == 13) {
    const _462_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["head"];
    const _463_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_18 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _462_0, ["tail"]: _463_0}}}}});
    const _x_19 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _462_0, ["tail"]: _463_0}}}}});
    return run_jump($vm$register_target$, [run_loop($Bool$and$((_x_18 > 0), (_x_19 < 32))), _mem_0, word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _462_0, ["tail"]: _463_0}}}}})]);
  } else if (_op_0 == 153) {
    return {$: "Tuple", ["fst"]: _mem_0, ["snd"]: run_loop($vm$check$((_ip_0 < 65535), {$: "Address"}))};
  } else if ((_op_0 & 7) == 1) {
    const _570_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["head"];
    const _571_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"];
    const _x_20 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _570_0, ["tail"]: _571_0}}}});
    const _x_21 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _570_0, ["tail"]: _571_0}}}});
    return run_jump($vm$register_target$, [run_loop($Bool$and$((_x_20 > 0), (_x_21 < 32))), _mem_0, word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _570_0, ["tail"]: _571_0}}}})]);
  } else if (_op_0 == 132) {
    const _x_22 = run_loop($vm$top$(_ds_0));
    return {$: "Tuple", ["fst"]: _mem_0, ["snd"]: run_loop($vm$check$((_x_22 !== 0), {$: "ZeroDivisor"}))};
  } else if ((_op_0 & 31) == 4) {
    const _636_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _637_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_23 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _636_0, ["tail"]: _637_0}}}}}});
    const _x_24 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _636_0, ["tail"]: _637_0}}}}}});
    return run_jump($vm$register_target$, [run_loop($Bool$and$((_x_23 > 0), (_x_24 < 32))), _mem_0, word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _636_0, ["tail"]: _637_0}}}}}})]);
  } else if (_op_0 == 148) {
    return {$: "Tuple", ["fst"]: _mem_0, ["snd"]: run_loop($vm$check$(run_loop($vm$address_ok$(run_loop($vm$top$(_ds_0)), 1)), {$: "Address"}))};
  } else if ((_op_0 & 31) == 20) {
    const _690_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _691_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_25 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _690_0, ["tail"]: _691_0}}}}}});
    const _x_26 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _690_0, ["tail"]: _691_0}}}}}});
    return run_jump($vm$register_target$, [run_loop($Bool$and$((_x_25 > 0), (_x_26 < 32))), _mem_0, word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _690_0, ["tail"]: _691_0}}}}}})]);
  } else if (_op_0 == 156) {
    const _x_27 = run_loop($vm$top$(_ds_0));
    return run_jump($vm$branch_check$, [(_x_27 === 0), _mem_0, _ip_0]);
  } else if ((_op_0 & 15) == 12) {
    const _744_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["head"];
    const _745_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_28 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _744_0, ["tail"]: _745_0}}}}});
    const _x_29 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _744_0, ["tail"]: _745_0}}}}});
    return run_jump($vm$register_target$, [run_loop($Bool$and$((_x_28 > 0), (_x_29 < 32))), _mem_0, word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _744_0, ["tail"]: _745_0}}}}})]);
  } else if (_op_0 == 152) {
    return {$: "Tuple", ["fst"]: _mem_0, ["snd"]: run_loop($vm$check$(run_loop($vm$address_ok$(run_loop($vm$top$(_ds_0)), 1)), {$: "Address"}))};
  } else if ((_op_0 & 7) == 0) {
    const _800_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["head"];
    const _801_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"];
    const _x_30 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _800_0, ["tail"]: _801_0}}}});
    const _x_31 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _800_0, ["tail"]: _801_0}}}});
    return run_jump($vm$register_target$, [run_loop($Bool$and$((_x_30 > 0), (_x_31 < 32))), _mem_0, word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _800_0, ["tail"]: _801_0}}}})]);
  } else if (_op_0 == 146) {
    return {$: "Tuple", ["fst"]: _mem_0, ["snd"]: run_loop($vm$check$(run_loop($vm$address_ok$(run_loop($vm$top$(_ds_0)), 1)), {$: "Address"}))};
  } else if ((_op_0 & 15) == 2) {
    const _862_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["head"];
    const _863_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_32 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _862_0, ["tail"]: _863_0}}}}});
    const _x_33 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _862_0, ["tail"]: _863_0}}}}});
    return run_jump($vm$register_target$, [run_loop($Bool$and$((_x_32 > 0), (_x_33 < 32))), _mem_0, word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _862_0, ["tail"]: _863_0}}}}})]);
  } else if (_op_0 == 154) {
    return run_jump($vm$check_target$, [(_ip_0 < 65532), _mem_0, ((_ip_0 + 1) >>> 0)]);
  } else if ((_op_0 & 15) == 10) {
    const _918_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["head"];
    const _919_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_34 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _918_0, ["tail"]: _919_0}}}}});
    const _x_35 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _918_0, ["tail"]: _919_0}}}}});
    return run_jump($vm$register_target$, [run_loop($Bool$and$((_x_34 > 0), (_x_35 < 32))), _mem_0, word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _918_0, ["tail"]: _919_0}}}}})]);
  } else if (_op_0 == 150) {
    return {$: "Tuple", ["fst"]: _mem_0, ["snd"]: run_loop($vm$check$((_ip_0 < 65535), {$: "Address"}))};
  } else if ((_op_0 & 15) == 6) {
    const _976_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["head"];
    const _977_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_36 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _976_0, ["tail"]: _977_0}}}}});
    const _x_37 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _976_0, ["tail"]: _977_0}}}}});
    return run_jump($vm$register_target$, [run_loop($Bool$and$((_x_36 > 0), (_x_37 < 32))), _mem_0, word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _976_0, ["tail"]: _977_0}}}}})]);
  } else if (_op_0 == 158) {
    const _addr_0 = run_loop($vm$top$(_cs_0));
    const _x_38 = (_addr_0 === 0);
    const _x_39 = run_loop($vm$code_ok$(_addr_0));
    return {$: "Tuple", ["fst"]: _mem_0, ["snd"]: run_loop($vm$check$((_x_38 || _x_39), {$: "Address"}))};
  } else {
    const _1032_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["head"];
    const _1033_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_40 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _1032_0, ["tail"]: _1033_0}}}}});
    const _x_41 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _1032_0, ["tail"]: _1033_0}}}}});
    return run_jump($vm$register_target$, [run_loop($Bool$and$((_x_40 > 0), (_x_41 < 32))), _mem_0, word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _1032_0, ["tail"]: _1033_0}}}}})]);
  }
}

function $vm$commit$(_reason_0, _op_0, _vm_0) {
  if (_reason_0.$ === "Clear") {
    return run_jump($vm$microcode$, [_op_0, _vm_0]);
  } else {
    return run_jump($vm$fault$, [_vm_0, _reason_0]);
  }
}

function $text$byte_other$(_reg_0, _x_0) {
  if (_reg_0) {
    return run_jump($text$register_name$, [(32 === 0 ? 0 : (_x_0 / 32) >>> 0), (32 === 0 ? _x_0 : _x_0 % 32)]);
  } else {
    return run_jump($Bool$pick$, [(_x_0 < 160), run_loop($text$name_at$(run_loop($text$core_names$()), ((_x_0 - 128) >>> 0))), run_loop($text$hex_fixed$(2n, _x_0, ""))]);
  }
}

function $Map$set$fin$go$(_m_0, _key_0, _x_0, _r_0) {
  const _t_0 = _r_0["fst"];
  const _keyb2_0 = _t_0["fst"];
  const _k2_0 = _t_0["snd"];
  const _t_1 = _r_0["snd"];
  if (_t_1.$ === "LT") {
    return run_jump($Map$ins$, [_m_0, _key_0, _x_0, run_loop($Map$diff$(_keyb2_0, _k2_0))]);
  } else if (_t_1.$ === "EQ") {
    return run_jump($Map$put$, [_m_0, _key_0, _x_0]);
  } else {
    return run_jump($Map$ins$, [_m_0, _key_0, _x_0, run_loop($Map$diff$(_keyb2_0, _k2_0))]);
  }
}

function $Map$lo$(_p2_0, _hi_0, _r0_0) {
  const _lo2_0 = _r0_0["fst"];
  const _r_0 = _r0_0["snd"];
  return {$: "Tuple", ["fst"]: {$: "MNode", ["pos"]: _p2_0, ["lo"]: _lo2_0, ["hi"]: _hi_0}, ["snd"]: _r_0};
}

function $Map$hi$(_p2_0, _lo_0, _r0_0) {
  const _hi2_0 = _r0_0["fst"];
  const _r_0 = _r0_0["snd"];
  return {$: "Tuple", ["fst"]: {$: "MNode", ["pos"]: _p2_0, ["lo"]: _lo_0, ["hi"]: _hi2_0}, ["snd"]: _r_0};
}

function $Map$bit$at$(_key_0, _co_0) {
  const _ci_0 = _co_0["fst"];
  const _off_0 = _co_0["snd"];
  return run_jump($Map$bit$go$, [_key_0, _ci_0, _off_0]);
}

function $Cmp$is_eq$(_c_0) {
  if (_c_0.$ === "LT") {
    return false;
  } else if (_c_0.$ === "EQ") {
    return true;
  } else {
    return false;
  }
}

function $String$cmp$fin$(_t1_0, _t2_0, _hc_0) {
  const _t_0 = _hc_0["fst"];
  const _h1b_0 = _t_0["fst"];
  const _h2b_0 = _t_0["snd"];
  const _t_1 = _hc_0["snd"];
  if (_t_1.$ === "LT") {
    return {$: "Tuple", ["fst"]: {$: "Tuple", ["fst"]: (_h1b_0 + _t1_0), ["snd"]: (_h2b_0 + _t2_0)}, ["snd"]: {$: "LT"}};
  } else if (_t_1.$ === "EQ") {
    return run_jump($String$cmp$rec$, [_h1b_0, _h2b_0, run_loop($String$cmp$(_t1_0, _t2_0))]);
  } else {
    return {$: "Tuple", ["fst"]: {$: "Tuple", ["fst"]: (_h1b_0 + _t1_0), ["snd"]: (_h2b_0 + _t2_0)}, ["snd"]: {$: "GT"}};
  }
}

function $Char$cmp$(_a_0, _b_0) {
  const _x_0 = _a_0.codePointAt(0);
  const _y_0 = _b_0.codePointAt(0);
  return {$: "Tuple", ["fst"]: {$: "Tuple", ["fst"]: char_new(_x_0), ["snd"]: char_new(_y_0)}, ["snd"]: cmp_new(_x_0, _y_0)};
}

function $emit_call$(_vm_0, _addr_0) {
  const _vm_1 = run_loop($emit$(_vm_0, 157));
  const _vm_2 = run_loop($emit$(_vm_1, _addr_0));
  const _vm_3 = run_loop($emit$(_vm_2, (8n >= 32n ? 0 : (_addr_0 >>> Number(8n)) >>> 0)));
  const _vm_4 = run_loop($emit$(_vm_3, (16n >= 32n ? 0 : (_addr_0 >>> Number(16n)) >>> 0)));
  return run_jump($emit$, [_vm_4, (24n >= 32n ? 0 : (_addr_0 >>> Number(24n)) >>> 0)]);
}

function $invoke_valid$(_vm_0, _addr_0) {
  const _mem_0 = _vm_0["mem"];
  const _ds_0 = _vm_0["ds"];
  const _cs_0 = _vm_0["cs"];
  const _ip_0 = _vm_0["ip"];
  const _control_0 = _vm_0["control"];
  const _output_0 = _vm_0["output"];
  return run_jump($vm$state$, [run_loop($vm$run$(BigInt(1000000), {$: "VM", ["mem"]: _mem_0, ["ds"]: _ds_0, ["cs"]: {$: "Con", ["head"]: 0, ["tail"]: _cs_0}, ["ip"]: _addr_0, ["control"]: {$: "Control", ["mode"]: {$: "Ru"}, ["fault"]: {$: "Clear"}, ["at"]: 0}, ["output"]: _output_0})), run_clo((_x_0) => {
  return run_clo((_x_1) => {
  return run_clo((_x_2) => {
  return run_clo((_x_3) => {
  return run_clo((_x_4) => {
  return run_clo((_x_5) => {
  return run_jump($invoke_finish$, [{$: "VM", ["mem"]: _x_0, ["ds"]: _x_1, ["cs"]: _x_2, ["ip"]: _x_3, ["control"]: _x_4, ["output"]: _x_5}, _ip_0]);
});
});
});
});
});
})]);
}

function $vm$check$(_ok_0, _reason_0) {
  if (_ok_0) {
    return {$: "Clear"};
  } else {
    return _reason_0;
  }
}

function $vm$top$(_xs_0) {
  if (_xs_0.$ === "Nil") {
    return 0;
  } else {
    const _h_0 = _xs_0["head"];
    const _t_0 = _xs_0["tail"];
    return _h_0;
  }
}

function $vm$register_target$(_invoke_0, _mem_0, _op_0) {
  if (_invoke_0) {
    return run_jump($vm$check_target$, [true, _mem_0, (Math.imul(_op_0, 4) >>> 0)]);
  } else {
    return {$: "Tuple", ["fst"]: _mem_0, ["snd"]: {$: "Clear"}};
  }
}

function $vm$address_ok$(_addr_0, _width_0) {
  const _x_0 = ((65536 - _width_0) >>> 0);
  return (_addr_0 <= _x_0);
}

function $vm$check_hop$(_ok_0, _mem_0, _ip_0) {
  if (!_ok_0) {
    return {$: "Tuple", ["fst"]: _mem_0, ["snd"]: {$: "Address"}};
  } else {
    const _x_0 = ((_ip_0 + 1) >>> 0);
    return run_jump($vm$pair$, [{$: "Tuple", fst: _mem_0, snd: _mem_0[_x_0 % _mem_0.length]}, run_clo((_x_1) => {
    return run_clo((_x_2) => {
    const _x_3 = run_loop($vm$signed_byte$(_x_2));
    return {$: "Tuple", ["fst"]: _x_1, ["snd"]: run_loop($vm$check$(run_loop($vm$code_ok$(((_ip_0 + _x_3) >>> 0))), {$: "Address"}))};
});
})]);
  }
}

function $vm$branch_check$(_taken_0, _mem_0, _ip_0) {
  if (_taken_0) {
    return run_jump($vm$check_hop$, [(_ip_0 < 65535), _mem_0, _ip_0]);
  } else {
    return {$: "Tuple", ["fst"]: _mem_0, ["snd"]: run_loop($vm$check$((_ip_0 < 65535), {$: "Address"}))};
  }
}

function $vm$next_counter$(_x_0) {
  return run_jump($vm$pick$, [run_loop($vm$signed_lt$(0, _x_0)), ((_x_0 - 1) >>> 0), _x_0]);
}

function $vm$magnitude$(_x_0) {
  return run_jump($vm$pick$, [run_loop($vm$negative$(_x_0)), run_loop($vm$negate$(_x_0)), _x_0]);
}

function $vm$check_target$(_ok_0, _mem_0, _addr_0) {
  if (!_ok_0) {
    return {$: "Tuple", ["fst"]: _mem_0, ["snd"]: {$: "Address"}};
  } else {
    return run_jump($vm$pair$, [run_loop($vm$read_cell$(_mem_0, _addr_0)), run_clo((_x_0) => {
    return run_clo((_x_1) => {
    return {$: "Tuple", ["fst"]: _x_0, ["snd"]: run_loop($vm$check$(run_loop($vm$code_ok$(_x_1)), {$: "Address"}))};
});
})]);
  }
}

function $vm$microcode$(_op_0, _vm_0) {
  if (_op_0 == 0) {
    return _vm_0;
  } else if (_op_0 == 128) {
    const _mem_0 = _vm_0["mem"];
    const _ds_0 = _vm_0["ds"];
    const _cs_0 = _vm_0["cs"];
    const _ip_0 = _vm_0["ip"];
    const _control_0 = _vm_0["control"];
    const _output_0 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_0)), run_clo((_x_0) => {
    return run_clo((_x_1) => {
    return run_jump($vm$pair$, [run_loop($vm$pop$(_x_1)), run_clo((_x_2) => {
    return run_clo((_x_3) => {
    return {$: "VM", ["mem"]: _mem_0, ["ds"]: {$: "Con", ["head"]: ((_x_2 + _x_0) >>> 0), ["tail"]: _x_3}, ["cs"]: _cs_0, ["ip"]: _ip_0, ["control"]: _control_0, ["output"]: _output_0};
});
})]);
});
})]);
  } else if (_op_0 == 192) {
    return run_jump($vm$push$, [_vm_0, 0]);
  } else if ((_op_0 & 31) == 0) {
    const _928_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _929_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_4 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _928_0, ["tail"]: _929_0}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_4 < 128), word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _928_0, ["tail"]: _929_0}}}}}}), _vm_0]);
  } else if (_op_0 == 144) {
    const _mem_1 = _vm_0["mem"];
    const _ds_1 = _vm_0["ds"];
    const _cs_1 = _vm_0["cs"];
    const _ip_1 = _vm_0["ip"];
    const _control_1 = _vm_0["control"];
    const _output_1 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_1)), run_clo((_x_5) => {
    return run_clo((_x_6) => {
    return {$: "VM", ["mem"]: _mem_1, ["ds"]: _x_6, ["cs"]: {$: "Con", ["head"]: _x_5, ["tail"]: _cs_1}, ["ip"]: _ip_1, ["control"]: _control_1, ["output"]: _output_1};
});
})]);
  } else if ((_op_0 & 31) == 16) {
    const _1080_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _1081_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_7 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _1080_0, ["tail"]: _1081_0}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_7 < 128), word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _1080_0, ["tail"]: _1081_0}}}}}}), _vm_0]);
  } else if (_op_0 == 136) {
    const _mem_2 = _vm_0["mem"];
    const _ds_2 = _vm_0["ds"];
    const _cs_2 = _vm_0["cs"];
    const _ip_2 = _vm_0["ip"];
    const _control_2 = _vm_0["control"];
    const _output_2 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_2)), run_clo((_x_8) => {
    return run_clo((_x_9) => {
    return run_jump($vm$pair$, [run_loop($vm$pop$(_x_9)), run_clo((_x_10) => {
    return run_clo((_x_11) => {
    return {$: "VM", ["mem"]: _mem_2, ["ds"]: {$: "Con", ["head"]: ((_x_10 ^ _x_8) >>> 0), ["tail"]: _x_11}, ["cs"]: _cs_2, ["ip"]: _ip_2, ["control"]: _control_2, ["output"]: _output_2};
});
})]);
});
})]);
  } else if ((_op_0 & 31) == 8) {
    const _1136_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _1137_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_12 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _1136_0, ["tail"]: _1137_0}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_12 < 128), word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _1136_0, ["tail"]: _1137_0}}}}}}), _vm_0]);
  } else if (_op_0 == 152) {
    const _mem_3 = _vm_0["mem"];
    const _ds_3 = _vm_0["ds"];
    const _cs_3 = _vm_0["cs"];
    const _ip_3 = _vm_0["ip"];
    const _control_3 = _vm_0["control"];
    const _output_3 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_3)), run_clo((_x_13) => {
    return run_clo((_x_14) => {
    return run_jump($vm$pair$, [{$: "Tuple", fst: _mem_3, snd: _mem_3[_x_13 % _mem_3.length]}, run_clo((_x_15) => {
    return run_clo((_x_16) => {
    return {$: "VM", ["mem"]: _x_15, ["ds"]: {$: "Con", ["head"]: run_loop($vm$signed_byte$(_x_16)), ["tail"]: _x_14}, ["cs"]: _cs_3, ["ip"]: _ip_3, ["control"]: _control_3, ["output"]: _output_3};
});
})]);
});
})]);
  } else if ((_op_0 & 63) == 24) {
    const _1192_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _1193_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_17 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _1192_0, ["tail"]: _1193_0}}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_17 < 128), word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _1192_0, ["tail"]: _1193_0}}}}}}}), _vm_0]);
  } else if (_op_0 == 248) {
    return run_jump($vm$push$, [_vm_0, 4]);
  } else if ((_op_0 & 63) == 56) {
    const _1244_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _1245_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_18 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _1244_0, ["tail"]: _1245_0}}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_18 < 128), word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _1244_0, ["tail"]: _1245_0}}}}}}}), _vm_0]);
  } else if (_op_0 == 132) {
    const _mem_4 = _vm_0["mem"];
    const _ds_4 = _vm_0["ds"];
    const _cs_4 = _vm_0["cs"];
    const _ip_4 = _vm_0["ip"];
    const _control_4 = _vm_0["control"];
    const _output_4 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_4)), run_clo((_x_19) => {
    return run_clo((_x_20) => {
    return run_jump($vm$pair$, [run_loop($vm$pop$(_x_20)), run_clo((_x_21) => {
    return run_clo((_x_22) => {
    return {$: "VM", ["mem"]: _mem_4, ["ds"]: {$: "Con", ["head"]: run_loop($vm$remainder$(_x_21, _x_19)), ["tail"]: _x_22}, ["cs"]: _cs_4, ["ip"]: _ip_4, ["control"]: _control_4, ["output"]: _output_4};
});
})]);
});
})]);
  } else if ((_op_0 & 31) == 4) {
    const _1300_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _1301_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_23 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _1300_0, ["tail"]: _1301_0}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_23 < 128), word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _1300_0, ["tail"]: _1301_0}}}}}}), _vm_0]);
  } else if (_op_0 == 148) {
    const _mem_5 = _vm_0["mem"];
    const _ds_5 = _vm_0["ds"];
    const _cs_5 = _vm_0["cs"];
    const _ip_5 = _vm_0["ip"];
    const _control_5 = _vm_0["control"];
    const _output_5 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_5)), run_clo((_x_24) => {
    return run_clo((_x_25) => {
    return run_jump($vm$pair$, [run_loop($vm$pop$(_x_25)), run_clo((_x_26) => {
    return run_clo((_x_27) => {
    const _x_28 = run_loop($vm$low_byte$(_x_26));
    const _mem_6 = (_mem_5[_x_24 % _mem_5.length] = _x_28, _mem_5);
    return {$: "VM", ["mem"]: _mem_6, ["ds"]: _x_27, ["cs"]: _cs_5, ["ip"]: _ip_5, ["control"]: _control_5, ["output"]: _output_5};
});
})]);
});
})]);
  } else if ((_op_0 & 31) == 20) {
    const _1354_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _1355_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_29 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _1354_0, ["tail"]: _1355_0}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_29 < 128), word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _1354_0, ["tail"]: _1355_0}}}}}}), _vm_0]);
  } else if (_op_0 == 140) {
    const _mem_7 = _vm_0["mem"];
    const _ds_6 = _vm_0["ds"];
    const _cs_6 = _vm_0["cs"];
    const _ip_6 = _vm_0["ip"];
    const _control_6 = _vm_0["control"];
    const _output_6 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_6)), run_clo((_x_30) => {
    return run_clo((_x_31) => {
    return {$: "VM", ["mem"]: _mem_7, ["ds"]: {$: "Con", ["head"]: _x_30, ["tail"]: {$: "Con", ["head"]: _x_30, ["tail"]: _x_31}}, ["cs"]: _cs_6, ["ip"]: _ip_6, ["control"]: _control_6, ["output"]: _output_6};
});
})]);
  } else if ((_op_0 & 31) == 12) {
    const _1410_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _1411_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_32 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _1410_0, ["tail"]: _1411_0}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_32 < 128), word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _1410_0, ["tail"]: _1411_0}}}}}}), _vm_0]);
  } else if (_op_0 == 156) {
    const _mem_8 = _vm_0["mem"];
    const _ds_7 = _vm_0["ds"];
    const _cs_7 = _vm_0["cs"];
    const _ip_7 = _vm_0["ip"];
    const _control_7 = _vm_0["control"];
    const _output_7 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_7)), run_clo((_x_33) => {
    return run_clo((_x_34) => {
    return run_jump($vm$pair$, [run_loop($vm$conditional_hop$((_x_33 === 0), _mem_8, _ip_7)), run_clo((_x_35) => {
    return run_clo((_x_36) => {
    return {$: "VM", ["mem"]: _x_35, ["ds"]: _x_34, ["cs"]: _cs_7, ["ip"]: _x_36, ["control"]: _control_7, ["output"]: _output_7};
});
})]);
});
})]);
  } else if ((_op_0 & 31) == 28) {
    const _1464_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _1465_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_37 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _1464_0, ["tail"]: _1465_0}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_37 < 128), word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _1464_0, ["tail"]: _1465_0}}}}}}), _vm_0]);
  } else if (_op_0 == 130) {
    const _mem_9 = _vm_0["mem"];
    const _ds_8 = _vm_0["ds"];
    const _cs_8 = _vm_0["cs"];
    const _ip_8 = _vm_0["ip"];
    const _control_8 = _vm_0["control"];
    const _output_8 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_8)), run_clo((_x_38) => {
    return run_clo((_x_39) => {
    return run_jump($vm$pair$, [run_loop($vm$pop$(_x_39)), run_clo((_x_40) => {
    return run_clo((_x_41) => {
    return {$: "VM", ["mem"]: _mem_9, ["ds"]: {$: "Con", ["head"]: (Math.imul(_x_40, _x_38) >>> 0), ["tail"]: _x_41}, ["cs"]: _cs_8, ["ip"]: _ip_8, ["control"]: _control_8, ["output"]: _output_8};
});
})]);
});
})]);
  } else if ((_op_0 & 31) == 2) {
    const _1524_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _1525_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_42 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _1524_0, ["tail"]: _1525_0}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_42 < 128), word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _1524_0, ["tail"]: _1525_0}}}}}}), _vm_0]);
  } else if (_op_0 == 146) {
    const _mem_10 = _vm_0["mem"];
    const _ds_9 = _vm_0["ds"];
    const _cs_9 = _vm_0["cs"];
    const _ip_9 = _vm_0["ip"];
    const _control_9 = _vm_0["control"];
    const _output_9 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_9)), run_clo((_x_43) => {
    return run_clo((_x_44) => {
    return run_jump($vm$pair$, [{$: "Tuple", fst: _mem_10, snd: _mem_10[_x_43 % _mem_10.length]}, run_clo((_x_45) => {
    return run_clo((_x_46) => {
    return {$: "VM", ["mem"]: _x_45, ["ds"]: {$: "Con", ["head"]: _x_46, ["tail"]: _x_44}, ["cs"]: _cs_9, ["ip"]: _ip_9, ["control"]: _control_9, ["output"]: _output_9};
});
})]);
});
})]);
  } else if ((_op_0 & 31) == 18) {
    const _1578_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _1579_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_47 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _1578_0, ["tail"]: _1579_0}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_47 < 128), word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _1578_0, ["tail"]: _1579_0}}}}}}), _vm_0]);
  } else if (_op_0 == 138) {
    const _mem_11 = _vm_0["mem"];
    const _ds_10 = _vm_0["ds"];
    const _cs_10 = _vm_0["cs"];
    const _ip_10 = _vm_0["ip"];
    const _control_10 = _vm_0["control"];
    const _output_10 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_10)), run_clo((_x_48) => {
    return run_clo((_x_49) => {
    return run_jump($vm$pair$, [run_loop($vm$pop$(_x_49)), run_clo((_x_50) => {
    return run_clo((_x_51) => {
    return {$: "VM", ["mem"]: _mem_11, ["ds"]: {$: "Con", ["head"]: run_loop($vm$truth$((_x_50 === _x_48))), ["tail"]: _x_51}, ["cs"]: _cs_10, ["ip"]: _ip_10, ["control"]: _control_10, ["output"]: _output_10};
});
})]);
});
})]);
  } else if ((_op_0 & 31) == 10) {
    const _1634_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _1635_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_52 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _1634_0, ["tail"]: _1635_0}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_52 < 128), word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _1634_0, ["tail"]: _1635_0}}}}}}), _vm_0]);
  } else if (_op_0 == 154) {
    const _mem_12 = _vm_0["mem"];
    const _ds_11 = _vm_0["ds"];
    const _cs_11 = _vm_0["cs"];
    const _ip_11 = _vm_0["ip"];
    const _control_11 = _vm_0["control"];
    const _output_11 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$read_cell$(_mem_12, ((_ip_11 + 1) >>> 0))), run_clo((_x_53) => {
    return run_clo((_x_54) => {
    return {$: "VM", ["mem"]: _x_53, ["ds"]: _ds_11, ["cs"]: _cs_11, ["ip"]: run_loop($vm$jump$(_x_54)), ["control"]: _control_11, ["output"]: _output_11};
});
})]);
  } else if ((_op_0 & 31) == 26) {
    const _1688_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _1689_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_55 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _1688_0, ["tail"]: _1689_0}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_55 < 128), word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _1688_0, ["tail"]: _1689_0}}}}}}), _vm_0]);
  } else if (_op_0 == 134) {
    const _mem_13 = _vm_0["mem"];
    const _ds_12 = _vm_0["ds"];
    const _cs_12 = _vm_0["cs"];
    const _ip_12 = _vm_0["ip"];
    const _control_12 = _vm_0["control"];
    const _output_12 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_12)), run_clo((_x_56) => {
    return run_clo((_x_57) => {
    return run_jump($vm$pair$, [run_loop($vm$pop$(_x_57)), run_clo((_x_58) => {
    return run_clo((_x_59) => {
    return {$: "VM", ["mem"]: _mem_13, ["ds"]: {$: "Con", ["head"]: ((_x_58 & _x_56) >>> 0), ["tail"]: _x_59}, ["cs"]: _cs_12, ["ip"]: _ip_12, ["control"]: _control_12, ["output"]: _output_12};
});
})]);
});
})]);
  } else if ((_op_0 & 31) == 6) {
    const _1746_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _1747_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_60 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _1746_0, ["tail"]: _1747_0}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_60 < 128), word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _1746_0, ["tail"]: _1747_0}}}}}}), _vm_0]);
  } else if (_op_0 == 150) {
    const _mem_14 = _vm_0["mem"];
    const _ds_13 = _vm_0["ds"];
    const _cs_13 = _vm_0["cs"];
    const _ip_13 = _vm_0["ip"];
    const _control_13 = _vm_0["control"];
    const _output_13 = _vm_0["output"];
    const _x_61 = ((_ip_13 + 1) >>> 0);
    return run_jump($vm$pair$, [{$: "Tuple", fst: _mem_14, snd: _mem_14[_x_61 % _mem_14.length]}, run_clo((_x_62) => {
    return run_clo((_x_63) => {
    return {$: "VM", ["mem"]: _x_62, ["ds"]: {$: "Con", ["head"]: _x_63, ["tail"]: _ds_13}, ["cs"]: _cs_13, ["ip"]: ((_ip_13 + 1) >>> 0), ["control"]: _control_13, ["output"]: _output_13};
});
})]);
  } else if ((_op_0 & 63) == 22) {
    const _1802_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _1803_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_64 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _1802_0, ["tail"]: _1803_0}}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_64 < 128), word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _1802_0, ["tail"]: _1803_0}}}}}}}), _vm_0]);
  } else if (_op_0 == 246) {
    return run_jump($vm$push$, [_vm_0, 2]);
  } else if ((_op_0 & 63) == 54) {
    const _1854_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _1855_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_65 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _1854_0, ["tail"]: _1855_0}}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_65 < 128), word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _1854_0, ["tail"]: _1855_0}}}}}}}), _vm_0]);
  } else if (_op_0 == 142) {
    const _mem_15 = _vm_0["mem"];
    const _ds_14 = _vm_0["ds"];
    const _cs_14 = _vm_0["cs"];
    const _ip_14 = _vm_0["ip"];
    const _control_14 = _vm_0["control"];
    const _output_14 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_14)), run_clo((_x_66) => {
    return run_clo((_x_67) => {
    return run_jump($vm$pair$, [run_loop($vm$pop$(_x_67)), run_clo((_x_68) => {
    return run_clo((_x_69) => {
    return {$: "VM", ["mem"]: _mem_15, ["ds"]: {$: "Con", ["head"]: _x_68, ["tail"]: {$: "Con", ["head"]: _x_66, ["tail"]: {$: "Con", ["head"]: _x_68, ["tail"]: _x_69}}}, ["cs"]: _cs_14, ["ip"]: _ip_14, ["control"]: _control_14, ["output"]: _output_14};
});
})]);
});
})]);
  } else if ((_op_0 & 31) == 14) {
    const _1908_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _1909_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_70 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _1908_0, ["tail"]: _1909_0}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_70 < 128), word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _1908_0, ["tail"]: _1909_0}}}}}}), _vm_0]);
  } else if (_op_0 == 158) {
    const _mem_16 = _vm_0["mem"];
    const _ds_15 = _vm_0["ds"];
    const _cs_15 = _vm_0["cs"];
    const _ip_15 = _vm_0["ip"];
    const _control_15 = _vm_0["control"];
    const _output_15 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_cs_15)), run_clo((_x_71) => {
    return run_clo((_x_72) => {
    return {$: "VM", ["mem"]: _mem_16, ["ds"]: _ds_15, ["cs"]: _x_72, ["ip"]: run_loop($vm$pick$((_x_71 === 0), _ip_15, ((_x_71 - 1) >>> 0))), ["control"]: run_loop($vm$return_control$((_x_71 === 0), _control_15)), ["output"]: _output_15};
});
})]);
  } else if ((_op_0 & 63) == 30) {
    const _1964_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _1965_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_73 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _1964_0, ["tail"]: _1965_0}}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_73 < 128), word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _1964_0, ["tail"]: _1965_0}}}}}}}), _vm_0]);
  } else if (_op_0 == 254) {
    const _mem_17 = _vm_0["mem"];
    const _ds_16 = _vm_0["ds"];
    const _cs_16 = _vm_0["cs"];
    const _ip_16 = _vm_0["ip"];
    const _control_16 = _vm_0["control"];
    const _output_16 = _vm_0["output"];
    return {$: "VM", ["mem"]: _mem_17, ["ds"]: _ds_16, ["cs"]: _cs_16, ["ip"]: _ip_16, ["control"]: {$: "Control", ["mode"]: {$: "Ti"}, ["fault"]: {$: "Clear"}, ["at"]: 0}, ["output"]: _output_16};
  } else if ((_op_0 & 63) == 62) {
    const _2016_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _2017_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_74 = word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _2016_0, ["tail"]: _2017_0}}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_74 < 128), word_to_u32({$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _2016_0, ["tail"]: _2017_0}}}}}}}), _vm_0]);
  } else if (_op_0 == 129) {
    const _mem_18 = _vm_0["mem"];
    const _ds_17 = _vm_0["ds"];
    const _cs_17 = _vm_0["cs"];
    const _ip_17 = _vm_0["ip"];
    const _control_17 = _vm_0["control"];
    const _output_17 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_17)), run_clo((_x_75) => {
    return run_clo((_x_76) => {
    return run_jump($vm$pair$, [run_loop($vm$pop$(_x_76)), run_clo((_x_77) => {
    return run_clo((_x_78) => {
    return {$: "VM", ["mem"]: _mem_18, ["ds"]: {$: "Con", ["head"]: ((_x_77 - _x_75) >>> 0), ["tail"]: _x_78}, ["cs"]: _cs_17, ["ip"]: _ip_17, ["control"]: _control_17, ["output"]: _output_17};
});
})]);
});
})]);
  } else if (_op_0 == 193) {
    return run_jump($vm$push$, [_vm_0, 1]);
  } else if ((_op_0 & 31) == 1) {
    const _2076_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _2077_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_79 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _2076_0, ["tail"]: _2077_0}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_79 < 128), word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _2076_0, ["tail"]: _2077_0}}}}}}), _vm_0]);
  } else if (_op_0 == 145) {
    const _mem_19 = _vm_0["mem"];
    const _ds_18 = _vm_0["ds"];
    const _cs_18 = _vm_0["cs"];
    const _ip_18 = _vm_0["ip"];
    const _control_18 = _vm_0["control"];
    const _output_18 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_cs_18)), run_clo((_x_80) => {
    return run_clo((_x_81) => {
    return {$: "VM", ["mem"]: _mem_19, ["ds"]: {$: "Con", ["head"]: _x_80, ["tail"]: _ds_18}, ["cs"]: _x_81, ["ip"]: _ip_18, ["control"]: _control_18, ["output"]: _output_18};
});
})]);
  } else if ((_op_0 & 31) == 17) {
    const _2180_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _2181_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_82 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _2180_0, ["tail"]: _2181_0}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_82 < 128), word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _2180_0, ["tail"]: _2181_0}}}}}}), _vm_0]);
  } else if (_op_0 == 137) {
    const _mem_20 = _vm_0["mem"];
    const _ds_19 = _vm_0["ds"];
    const _cs_19 = _vm_0["cs"];
    const _ip_19 = _vm_0["ip"];
    const _control_19 = _vm_0["control"];
    const _output_19 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_19)), run_clo((_x_83) => {
    return run_clo((_x_84) => {
    return {$: "VM", ["mem"]: _mem_20, ["ds"]: {$: "Con", ["head"]: (~_x_83 >>> 0), ["tail"]: _x_84}, ["cs"]: _cs_19, ["ip"]: _ip_19, ["control"]: _control_19, ["output"]: _output_19};
});
})]);
  } else if ((_op_0 & 31) == 9) {
    const _2236_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _2237_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_85 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _2236_0, ["tail"]: _2237_0}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_85 < 128), word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _2236_0, ["tail"]: _2237_0}}}}}}), _vm_0]);
  } else if (_op_0 == 153) {
    const _mem_21 = _vm_0["mem"];
    const _ds_20 = _vm_0["ds"];
    const _cs_20 = _vm_0["cs"];
    const _ip_20 = _vm_0["ip"];
    const _control_20 = _vm_0["control"];
    const _output_20 = _vm_0["output"];
    const _x_86 = ((_ip_20 + 1) >>> 0);
    return run_jump($vm$pair$, [{$: "Tuple", fst: _mem_21, snd: _mem_21[_x_86 % _mem_21.length]}, run_clo((_x_87) => {
    return run_clo((_x_88) => {
    return {$: "VM", ["mem"]: _x_87, ["ds"]: {$: "Con", ["head"]: run_loop($vm$signed_byte$(_x_88)), ["tail"]: _ds_20}, ["cs"]: _cs_20, ["ip"]: ((_ip_20 + 1) >>> 0), ["control"]: _control_20, ["output"]: _output_20};
});
})]);
  } else if ((_op_0 & 31) == 25) {
    const _2290_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _2291_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_89 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _2290_0, ["tail"]: _2291_0}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_89 < 128), word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _2290_0, ["tail"]: _2291_0}}}}}}), _vm_0]);
  } else if (_op_0 == 133) {
    const _mem_22 = _vm_0["mem"];
    const _ds_21 = _vm_0["ds"];
    const _cs_21 = _vm_0["cs"];
    const _ip_21 = _vm_0["ip"];
    const _control_21 = _vm_0["control"];
    const _output_21 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_21)), run_clo((_x_90) => {
    return run_clo((_x_91) => {
    return run_jump($vm$pair$, [run_loop($vm$pop$(_x_91)), run_clo((_x_92) => {
    return run_clo((_x_93) => {
    return {$: "VM", ["mem"]: _mem_22, ["ds"]: {$: "Con", ["head"]: run_loop($vm$shift$(_x_92, _x_90)), ["tail"]: _x_93}, ["cs"]: _cs_21, ["ip"]: _ip_21, ["control"]: _control_21, ["output"]: _output_21};
});
})]);
});
})]);
  } else if ((_op_0 & 31) == 5) {
    const _2348_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _2349_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_94 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _2348_0, ["tail"]: _2349_0}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_94 < 128), word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _2348_0, ["tail"]: _2349_0}}}}}}), _vm_0]);
  } else if (_op_0 == 149) {
    const _mem_23 = _vm_0["mem"];
    const _ds_22 = _vm_0["ds"];
    const _cs_22 = _vm_0["cs"];
    const _ip_22 = _vm_0["ip"];
    const _control_22 = _vm_0["control"];
    const _output_22 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_22)), run_clo((_x_95) => {
    return run_clo((_x_96) => {
    return run_jump($vm$pair$, [run_loop($vm$pop$(_x_96)), run_clo((_x_97) => {
    return run_clo((_x_98) => {
    return {$: "VM", ["mem"]: run_loop($vm$write_cell$(_mem_23, _x_95, _x_97)), ["ds"]: _x_98, ["cs"]: _cs_22, ["ip"]: _ip_22, ["control"]: _control_22, ["output"]: _output_22};
});
})]);
});
})]);
  } else if ((_op_0 & 31) == 21) {
    const _2402_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _2403_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_99 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _2402_0, ["tail"]: _2403_0}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_99 < 128), word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _2402_0, ["tail"]: _2403_0}}}}}}), _vm_0]);
  } else if (_op_0 == 141) {
    const _mem_24 = _vm_0["mem"];
    const _ds_23 = _vm_0["ds"];
    const _cs_23 = _vm_0["cs"];
    const _ip_23 = _vm_0["ip"];
    const _control_23 = _vm_0["control"];
    const _output_23 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_23)), run_clo((_x_100) => {
    return run_clo((_x_101) => {
    return run_jump($vm$pair$, [run_loop($vm$pop$(_x_101)), run_clo((_x_102) => {
    return run_clo((_x_103) => {
    return {$: "VM", ["mem"]: _mem_24, ["ds"]: {$: "Con", ["head"]: _x_102, ["tail"]: {$: "Con", ["head"]: _x_100, ["tail"]: _x_103}}, ["cs"]: _cs_23, ["ip"]: _ip_23, ["control"]: _control_23, ["output"]: _output_23};
});
})]);
});
})]);
  } else if ((_op_0 & 31) == 13) {
    const _2458_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _2459_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_104 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _2458_0, ["tail"]: _2459_0}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_104 < 128), word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _2458_0, ["tail"]: _2459_0}}}}}}), _vm_0]);
  } else if (_op_0 == 157) {
    const _mem_25 = _vm_0["mem"];
    const _ds_24 = _vm_0["ds"];
    const _cs_24 = _vm_0["cs"];
    const _ip_24 = _vm_0["ip"];
    const _control_24 = _vm_0["control"];
    const _output_24 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$read_cell$(_mem_25, ((_ip_24 + 1) >>> 0))), run_clo((_x_105) => {
    return run_clo((_x_106) => {
    return {$: "VM", ["mem"]: _x_105, ["ds"]: _ds_24, ["cs"]: {$: "Con", ["head"]: ((_ip_24 + 5) >>> 0), ["tail"]: _cs_24}, ["ip"]: run_loop($vm$jump$(_x_106)), ["control"]: _control_24, ["output"]: _output_24};
});
})]);
  } else if ((_op_0 & 63) == 29) {
    const _2514_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _2515_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_107 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _2514_0, ["tail"]: _2515_0}}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_107 < 128), word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _2514_0, ["tail"]: _2515_0}}}}}}}), _vm_0]);
  } else if (_op_0 == 253) {
    const _mem_26 = _vm_0["mem"];
    const _ds_25 = _vm_0["ds"];
    const _cs_25 = _vm_0["cs"];
    const _ip_25 = _vm_0["ip"];
    const _control_25 = _vm_0["control"];
    const _output_25 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_25)), run_clo((_x_108) => {
    return run_clo((_x_109) => {
    return run_jump($vm$device$, [_x_108, {$: "VM", ["mem"]: _mem_26, ["ds"]: _x_109, ["cs"]: _cs_25, ["ip"]: _ip_25, ["control"]: _control_25, ["output"]: _output_25}]);
});
})]);
  } else if ((_op_0 & 63) == 61) {
    const _2566_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _2567_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_110 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _2566_0, ["tail"]: _2567_0}}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_110 < 128), word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _2566_0, ["tail"]: _2567_0}}}}}}}), _vm_0]);
  } else if (_op_0 == 131) {
    const _mem_27 = _vm_0["mem"];
    const _ds_26 = _vm_0["ds"];
    const _cs_26 = _vm_0["cs"];
    const _ip_26 = _vm_0["ip"];
    const _control_26 = _vm_0["control"];
    const _output_26 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_26)), run_clo((_x_111) => {
    return run_clo((_x_112) => {
    return run_jump($vm$pair$, [run_loop($vm$pop$(_x_112)), run_clo((_x_113) => {
    return run_clo((_x_114) => {
    return {$: "VM", ["mem"]: _mem_27, ["ds"]: {$: "Con", ["head"]: run_loop($vm$divide$(_x_113, _x_111)), ["tail"]: _x_114}, ["cs"]: _cs_26, ["ip"]: _ip_26, ["control"]: _control_26, ["output"]: _output_26};
});
})]);
});
})]);
  } else if ((_op_0 & 31) == 3) {
    const _2624_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _2625_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_115 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _2624_0, ["tail"]: _2625_0}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_115 < 128), word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _2624_0, ["tail"]: _2625_0}}}}}}), _vm_0]);
  } else if (_op_0 == 147) {
    const _mem_28 = _vm_0["mem"];
    const _ds_27 = _vm_0["ds"];
    const _cs_27 = _vm_0["cs"];
    const _ip_27 = _vm_0["ip"];
    const _control_27 = _vm_0["control"];
    const _output_27 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_27)), run_clo((_x_116) => {
    return run_clo((_x_117) => {
    return run_jump($vm$pair$, [run_loop($vm$read_cell$(_mem_28, _x_116)), run_clo((_x_118) => {
    return run_clo((_x_119) => {
    return {$: "VM", ["mem"]: _x_118, ["ds"]: {$: "Con", ["head"]: _x_119, ["tail"]: _x_117}, ["cs"]: _cs_27, ["ip"]: _ip_27, ["control"]: _control_27, ["output"]: _output_27};
});
})]);
});
})]);
  } else if ((_op_0 & 31) == 19) {
    const _2678_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _2679_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_120 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _2678_0, ["tail"]: _2679_0}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_120 < 128), word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _2678_0, ["tail"]: _2679_0}}}}}}), _vm_0]);
  } else if (_op_0 == 139) {
    const _mem_29 = _vm_0["mem"];
    const _ds_28 = _vm_0["ds"];
    const _cs_28 = _vm_0["cs"];
    const _ip_28 = _vm_0["ip"];
    const _control_28 = _vm_0["control"];
    const _output_28 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_28)), run_clo((_x_121) => {
    return run_clo((_x_122) => {
    return run_jump($vm$pair$, [run_loop($vm$pop$(_x_122)), run_clo((_x_123) => {
    return run_clo((_x_124) => {
    return {$: "VM", ["mem"]: _mem_29, ["ds"]: {$: "Con", ["head"]: run_loop($vm$truth$(run_loop($vm$signed_lt$(_x_123, _x_121)))), ["tail"]: _x_124}, ["cs"]: _cs_28, ["ip"]: _ip_28, ["control"]: _control_28, ["output"]: _output_28};
});
})]);
});
})]);
  } else if ((_op_0 & 31) == 11) {
    const _2734_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _2735_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_125 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _2734_0, ["tail"]: _2735_0}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_125 < 128), word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _2734_0, ["tail"]: _2735_0}}}}}}), _vm_0]);
  } else if (_op_0 == 155) {
    const _mem_30 = _vm_0["mem"];
    const _ds_29 = _vm_0["ds"];
    const _cs_29 = _vm_0["cs"];
    const _ip_29 = _vm_0["ip"];
    const _control_29 = _vm_0["control"];
    const _output_29 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$hop$(_mem_30, _ip_29)), run_clo((_x_126) => {
    return run_clo((_x_127) => {
    return {$: "VM", ["mem"]: _x_126, ["ds"]: _ds_29, ["cs"]: _cs_29, ["ip"]: _x_127, ["control"]: _control_29, ["output"]: _output_29};
});
})]);
  } else if ((_op_0 & 31) == 27) {
    const _2788_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _2789_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_128 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _2788_0, ["tail"]: _2789_0}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_128 < 128), word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _2788_0, ["tail"]: _2789_0}}}}}}), _vm_0]);
  } else if (_op_0 == 135) {
    const _mem_31 = _vm_0["mem"];
    const _ds_30 = _vm_0["ds"];
    const _cs_30 = _vm_0["cs"];
    const _ip_30 = _vm_0["ip"];
    const _control_30 = _vm_0["control"];
    const _output_30 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_30)), run_clo((_x_129) => {
    return run_clo((_x_130) => {
    return run_jump($vm$pair$, [run_loop($vm$pop$(_x_130)), run_clo((_x_131) => {
    return run_clo((_x_132) => {
    return {$: "VM", ["mem"]: _mem_31, ["ds"]: {$: "Con", ["head"]: ((_x_131 | _x_129) >>> 0), ["tail"]: _x_132}, ["cs"]: _cs_30, ["ip"]: _ip_30, ["control"]: _control_30, ["output"]: _output_30};
});
})]);
});
})]);
  } else if ((_op_0 & 31) == 7) {
    const _2846_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _2847_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_133 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _2846_0, ["tail"]: _2847_0}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_133 < 128), word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _2846_0, ["tail"]: _2847_0}}}}}}), _vm_0]);
  } else if (_op_0 == 151) {
    const _mem_32 = _vm_0["mem"];
    const _ds_31 = _vm_0["ds"];
    const _cs_31 = _vm_0["cs"];
    const _ip_31 = _vm_0["ip"];
    const _control_31 = _vm_0["control"];
    const _output_31 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$read_cell$(_mem_32, ((_ip_31 + 1) >>> 0))), run_clo((_x_134) => {
    return run_clo((_x_135) => {
    return {$: "VM", ["mem"]: _x_134, ["ds"]: {$: "Con", ["head"]: _x_135, ["tail"]: _ds_31}, ["cs"]: _cs_31, ["ip"]: ((_ip_31 + 4) >>> 0), ["control"]: _control_31, ["output"]: _output_31};
});
})]);
  } else if ((_op_0 & 63) == 23) {
    const _2902_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _2903_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_136 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _2902_0, ["tail"]: _2903_0}}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_136 < 128), word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _2902_0, ["tail"]: _2903_0}}}}}}}), _vm_0]);
  } else if (_op_0 == 247) {
    return run_jump($vm$push$, [_vm_0, 4294967295]);
  } else if ((_op_0 & 63) == 55) {
    const _2954_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _2955_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_137 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _2954_0, ["tail"]: _2955_0}}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_137 < 128), word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _2954_0, ["tail"]: _2955_0}}}}}}}), _vm_0]);
  } else if (_op_0 == 143) {
    const _mem_33 = _vm_0["mem"];
    const _ds_32 = _vm_0["ds"];
    const _cs_32 = _vm_0["cs"];
    const _ip_32 = _vm_0["ip"];
    const _control_32 = _vm_0["control"];
    const _output_32 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_32)), run_clo((_x_138) => {
    return run_clo((_x_139) => {
    return {$: "VM", ["mem"]: _mem_33, ["ds"]: _x_139, ["cs"]: _cs_32, ["ip"]: _ip_32, ["control"]: _control_32, ["output"]: _output_32};
});
})]);
  } else if ((_op_0 & 31) == 15) {
    const _3008_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _3009_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_140 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _3008_0, ["tail"]: _3009_0}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_140 < 128), word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _3008_0, ["tail"]: _3009_0}}}}}}), _vm_0]);
  } else if (_op_0 == 159) {
    const _mem_34 = _vm_0["mem"];
    const _ds_33 = _vm_0["ds"];
    const _cs_33 = _vm_0["cs"];
    const _ip_33 = _vm_0["ip"];
    const _control_33 = _vm_0["control"];
    const _output_33 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_cs_33)), run_clo((_x_141) => {
    return run_clo((_x_142) => {
    const _n_0 = run_loop($vm$next_counter$(_x_141));
    return run_jump($vm$next_jump$, [(_n_0 === 0), _mem_34, _ds_33, _x_142, _n_0, _ip_33, _control_33, _output_33]);
});
})]);
  } else if ((_op_0 & 63) == 31) {
    const _3064_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _3065_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_143 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _3064_0, ["tail"]: _3065_0}}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_143 < 128), word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: false, ["tail"]: {$: "WCon", ["head"]: _3064_0, ["tail"]: _3065_0}}}}}}}), _vm_0]);
  } else if (_op_0 == 255) {
    const _mem_35 = _vm_0["mem"];
    const _ds_34 = _vm_0["ds"];
    const _cs_34 = _vm_0["cs"];
    const _ip_34 = _vm_0["ip"];
    const _control_34 = _vm_0["control"];
    const _output_34 = _vm_0["output"];
    return {$: "VM", ["mem"]: _mem_35, ["ds"]: _ds_34, ["cs"]: _cs_34, ["ip"]: _ip_34, ["control"]: {$: "Control", ["mode"]: {$: "Ti"}, ["fault"]: {$: "Clear"}, ["at"]: 0}, ["output"]: _output_34};
  } else {
    const _3116_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"]["head"];
    const _3117_0 = u32_to_word(_op_0)["tail"]["tail"]["tail"]["tail"]["tail"]["tail"]["tail"];
    const _x_144 = word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _3116_0, ["tail"]: _3117_0}}}}}}});
    return run_jump($vm$register_or_reserved$, [(_x_144 < 128), word_to_u32({$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: true, ["tail"]: {$: "WCon", ["head"]: _3116_0, ["tail"]: _3117_0}}}}}}}), _vm_0]);
  }
}

function $text$register_name$(_group_0, _r_0) {
  if (_group_0 == 0) {
    const _x_0 = (char_new(((64 + _r_0) >>> 0)) + "");
    return ("^" + _x_0);
  } else if ((_group_0 & 3) == 0) {
    const _12_0 = u32_to_word(_group_0)["tail"]["tail"]["head"];
    const _13_0 = u32_to_word(_group_0)["tail"]["tail"]["tail"];
    const _x_1 = (char_new(((64 + _r_0) >>> 0)) + "");
    return ("+" + _x_1);
  } else if (_group_0 == 2) {
    const _x_2 = (char_new(((64 + _r_0) >>> 0)) + "");
    return ("!" + _x_2);
  } else if ((_group_0 & 3) == 2) {
    const _72_0 = u32_to_word(_group_0)["tail"]["tail"]["head"];
    const _73_0 = u32_to_word(_group_0)["tail"]["tail"]["tail"];
    const _x_3 = (char_new(((64 + _r_0) >>> 0)) + "");
    return ("+" + _x_3);
  } else if (_group_0 == 1) {
    const _x_4 = (char_new(((64 + _r_0) >>> 0)) + "");
    return ("@" + _x_4);
  } else {
    const _132_0 = u32_to_word(_group_0)["tail"]["head"];
    const _133_0 = u32_to_word(_group_0)["tail"]["tail"];
    const _x_5 = (char_new(((64 + _r_0) >>> 0)) + "");
    return ("+" + _x_5);
  }
}

function $text$name_at$(_xs_0, _n_0) {
  if (_xs_0.$ === "Nil") {
    return "??";
  } else {
    const _h_0 = _xs_0["head"];
    const _t_0 = _xs_0["tail"];
    return run_jump($Bool$pick$, [(_n_0 === 0), _h_0, run_loop($text$name_at$(_t_0, ((_n_0 - 1) >>> 0)))]);
  }
}

function $Map$ins$(_m_0, _key_0, _x_0, _p_0) {
  if (_m_0.$ === "MTip") {
    return {$: "MLeaf", ["key"]: _key_0, ["val"]: _x_0};
  } else if (_m_0.$ === "MLeaf") {
    const _k_0 = _m_0["key"];
    const _v_0 = _m_0["val"];
    return run_jump($Map$ins$splice$, [_p_0, _key_0, _x_0, {$: "MLeaf", ["key"]: _k_0, ["val"]: _v_0}]);
  } else {
    const _pos_0 = _m_0["pos"];
    const _lo_0 = _m_0["lo"];
    const _hi_0 = _m_0["hi"];
    return run_jump($Map$ins$if$, [_key_0, _x_0, _lo_0, _hi_0, _pos_0, _p_0, (_pos_0 < _p_0)]);
  }
}

function $Map$diff$(_a_0, _b_0) {
  if (_a_0 === "") {
    if (_b_0 === "") {
      return 0n;
    } else {
      const _h_0 = (_b_0.codePointAt(0) > 0xFFFF ? _b_0.slice(0, 2) : _b_0[0]);
      const _t_0 = (_b_0.codePointAt(0) > 0xFFFF ? _b_0.slice(2) : _b_0.slice(1));
      return 0n;
    }
  } else {
    const _h_1 = (_a_0.codePointAt(0) > 0xFFFF ? _a_0.slice(0, 2) : _a_0[0]);
    const _t_1 = (_a_0.codePointAt(0) > 0xFFFF ? _a_0.slice(2) : _a_0.slice(1));
    if (_b_0 === "") {
      return 0n;
    } else {
      const _y_0 = (_b_0.codePointAt(0) > 0xFFFF ? _b_0.slice(0, 2) : _b_0[0]);
      const _yt_0 = (_b_0.codePointAt(0) > 0xFFFF ? _b_0.slice(2) : _b_0.slice(1));
      return run_jump($Map$diff$fin$, [_t_1, _yt_0, run_loop($Map$diff$step$(_h_1, _y_0))]);
    }
  }
}

function $Map$put$(_m_0, _key_0, _x_0) {
  if (_m_0.$ === "MTip") {
    return {$: "MLeaf", ["key"]: _key_0, ["val"]: _x_0};
  } else if (_m_0.$ === "MLeaf") {
    const _k_0 = _m_0["key"];
    const _v_0 = _m_0["val"];
    return {$: "MLeaf", ["key"]: _k_0, ["val"]: _x_0};
  } else {
    const _pos_0 = _m_0["pos"];
    const _lo_0 = _m_0["lo"];
    const _hi_0 = _m_0["hi"];
    return run_jump($Map$put$bit$, [_x_0, _pos_0, _lo_0, _hi_0, run_loop($Map$bit$(_key_0, _pos_0))]);
  }
}

function $Map$bit$go$(_key_0, _ci_0, _off_0) {
  if (_key_0 === "") {
    return {$: "Tuple", ["fst"]: "", ["snd"]: false};
  } else {
    const _c_0 = (_key_0.codePointAt(0) > 0xFFFF ? _key_0.slice(0, 2) : _key_0[0]);
    const _t_0 = (_key_0.codePointAt(0) > 0xFFFF ? _key_0.slice(2) : _key_0.slice(1));
    if (_ci_0 === 0n) {
      return run_jump($Map$bit$go$chr$, [_t_0, run_loop($Map$bit$chr$(_c_0, _off_0))]);
    } else {
      const _j_0 = (_ci_0 - 1n);
      return run_jump($Map$bit$go$rec$, [_c_0, run_loop($Map$bit$go$(_t_0, _j_0, _off_0))]);
    }
  }
}

function $String$cmp$rec$(_h1b_0, _h2b_0, _rr_0) {
  const _t_0 = _rr_0["fst"];
  const _t1b_0 = _t_0["fst"];
  const _t2b_0 = _t_0["snd"];
  const _r_0 = _rr_0["snd"];
  return {$: "Tuple", ["fst"]: {$: "Tuple", ["fst"]: (_h1b_0 + _t1b_0), ["snd"]: (_h2b_0 + _t2b_0)}, ["snd"]: _r_0};
}

function $invoke_finish$(_vm_0, _saved_0) {
  const _mem_0 = _vm_0["mem"];
  const _ds_0 = _vm_0["ds"];
  const _cs_0 = _vm_0["cs"];
  const _ip_0 = _vm_0["ip"];
  const _t_0 = _vm_0["control"];
  const _t_1 = _t_0["mode"];
  if (_t_1.$ === "Ti") {
    const _reason_0 = _t_0["fault"];
    const _at_0 = _t_0["at"];
    const _output_0 = _vm_0["output"];
    return {$: "VM", ["mem"]: _mem_0, ["ds"]: _ds_0, ["cs"]: _cs_0, ["ip"]: _saved_0, ["control"]: {$: "Control", ["mode"]: {$: "Ti"}, ["fault"]: _reason_0, ["at"]: _at_0}, ["output"]: _output_0};
  } else {
    const _reason_1 = _t_0["fault"];
    const _at_1 = _t_0["at"];
    const _output_1 = _vm_0["output"];
    return {$: "VM", ["mem"]: _mem_0, ["ds"]: _ds_0, ["cs"]: _cs_0, ["ip"]: _ip_0, ["control"]: {$: "Control", ["mode"]: _t_1, ["fault"]: _reason_1, ["at"]: _at_1}, ["output"]: _output_1};
  }
}

function $vm$signed_byte$(_x_0) {
  return run_jump($vm$pick$, [(_x_0 >= 128), ((_x_0 | 4294967040) >>> 0), _x_0]);
}

function $vm$signed_lt$(_a_0, _b_0) {
  const _x_0 = ((_a_0 ^ 2147483648) >>> 0);
  const _x_1 = ((_b_0 ^ 2147483648) >>> 0);
  return (_x_0 < _x_1);
}

function $vm$register_or_reserved$(_is_register_0, _op_0, _vm_0) {
  if (_is_register_0) {
    const _x_0 = (32 === 0 ? _op_0 : _op_0 % 32);
    return run_jump($vm$register_op$, [(32 === 0 ? 0 : (_op_0 / 32) >>> 0), (Math.imul(_x_0, 4) >>> 0), _vm_0]);
  } else {
    return _vm_0;
  }
}

function $vm$pop$(_xs_0) {
  if (_xs_0.$ === "Nil") {
    return {$: "Tuple", ["fst"]: 0, ["snd"]: {$: "Nil"}};
  } else {
    const _h_0 = _xs_0["head"];
    const _t_0 = _xs_0["tail"];
    return {$: "Tuple", ["fst"]: _h_0, ["snd"]: _t_0};
  }
}

function $vm$remainder$(_a_0, _b_0) {
  const _x_0 = run_loop($vm$magnitude$(_a_0));
  const _x_1 = run_loop($vm$magnitude$(_b_0));
  const _r_0 = (_x_1 === 0 ? _x_0 : _x_0 % _x_1);
  return run_jump($vm$pick$, [run_loop($vm$negative$(_a_0)), run_loop($vm$negate$(_r_0)), _r_0]);
}

function $vm$conditional_hop$(_zero_0, _mem_0, _ip_0) {
  if (_zero_0) {
    return run_jump($vm$hop$, [_mem_0, _ip_0]);
  } else {
    return {$: "Tuple", ["fst"]: _mem_0, ["snd"]: ((_ip_0 + 1) >>> 0)};
  }
}

function $vm$truth$(_b_0) {
  return run_jump($vm$pick$, [_b_0, 4294967295, 0]);
}

function $vm$jump$(_a_0) {
  const _x_0 = run_loop($vm$pick$(run_loop($vm$signed_lt$(_a_0, 256)), 256, _a_0));
  return ((_x_0 - 1) >>> 0);
}

function $vm$return_control$(_top_0, _control_0) {
  if (_top_0) {
    return {$: "Control", ["mode"]: {$: "Ti"}, ["fault"]: {$: "Clear"}, ["at"]: 0};
  } else {
    return _control_0;
  }
}

function $vm$shift$(_a_0, _b_0) {
  const _x_0 = run_loop($vm$magnitude$(_b_0));
  return run_jump($vm$shift_valid$, [(_x_0 < 32), _a_0, _b_0]);
}

function $vm$device$(_cmd_0, _vm_0) {
  if (_cmd_0 == 101) {
    const _mem_0 = _vm_0["mem"];
    const _ds_0 = _vm_0["ds"];
    const _cs_0 = _vm_0["cs"];
    const _ip_0 = _vm_0["ip"];
    const _control_0 = _vm_0["control"];
    const _output_0 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_0)), run_clo((_x_0) => {
    return run_clo((_x_1) => {
    const _x_2 = (char_new(_x_0) + "");
    return {$: "VM", ["mem"]: _mem_0, ["ds"]: _x_1, ["cs"]: _cs_0, ["ip"]: _ip_0, ["control"]: _control_0, ["output"]: (_output_0 + _x_2)};
});
})]);
  } else {
    const _41_0 = u32_to_word(_cmd_0)["head"];
    const _42_0 = u32_to_word(_cmd_0)["tail"];
    const _mem_1 = _vm_0["mem"];
    const _ds_1 = _vm_0["ds"];
    const _cs_1 = _vm_0["cs"];
    const _ip_1 = _vm_0["ip"];
    const _control_1 = _vm_0["control"];
    const _output_1 = _vm_0["output"];
    return {$: "VM", ["mem"]: _mem_1, ["ds"]: _ds_1, ["cs"]: _cs_1, ["ip"]: _ip_1, ["control"]: {$: "Control", ["mode"]: {$: "Na"}, ["fault"]: {$: "Device"}, ["at"]: _ip_1}, ["output"]: _output_1};
  }
}

function $vm$divide$(_a_0, _b_0) {
  const _x_0 = run_loop($vm$magnitude$(_a_0));
  const _x_1 = run_loop($vm$magnitude$(_b_0));
  const _q_0 = (_x_1 === 0 ? 0 : (_x_0 / _x_1) >>> 0);
  return run_jump($vm$pick$, [run_loop($vm$negative$(((_a_0 ^ _b_0) >>> 0))), run_loop($vm$negate$(_q_0)), _q_0]);
}

function $vm$hop$(_mem_0, _ip_0) {
  const _x_0 = ((_ip_0 + 1) >>> 0);
  return run_jump($vm$pair$, [{$: "Tuple", fst: _mem_0, snd: _mem_0[_x_0 % _mem_0.length]}, run_clo((_x_1) => {
  return run_clo((_x_2) => {
  const _x_3 = run_loop($vm$signed_byte$(_x_2));
  return {$: "Tuple", ["fst"]: _x_1, ["snd"]: run_loop($vm$jump$(((_ip_0 + _x_3) >>> 0)))};
});
})]);
}

function $vm$next_jump$(_zero_0, _mem_0, _ds_0, _cs_0, _counter_0, _ip_0, _control_0, _output_0) {
  if (_zero_0) {
    return {$: "VM", ["mem"]: _mem_0, ["ds"]: _ds_0, ["cs"]: _cs_0, ["ip"]: ((_ip_0 + 1) >>> 0), ["control"]: _control_0, ["output"]: _output_0};
  } else {
    return run_jump($vm$pair$, [run_loop($vm$hop$(_mem_0, _ip_0)), run_clo((_x_0) => {
    return run_clo((_x_1) => {
    return {$: "VM", ["mem"]: _x_0, ["ds"]: _ds_0, ["cs"]: {$: "Con", ["head"]: _counter_0, ["tail"]: _cs_0}, ["ip"]: _x_1, ["control"]: _control_0, ["output"]: _output_0};
});
})]);
  }
}

function $Map$ins$splice$(_p_0, _key_0, _x_0, _rest_0) {
  return run_jump($Map$ins$splice$bit$, [_x_0, _rest_0, _p_0, run_loop($Map$bit$(_key_0, _p_0))]);
}

function $Map$ins$if$(_key_0, _x_0, _lo_0, _hi_0, _p2_0, _pb_0, _t_0) {
  if (!_t_0) {
    return run_jump($Map$ins$splice$, [_pb_0, _key_0, _x_0, {$: "MNode", ["pos"]: _p2_0, ["lo"]: _lo_0, ["hi"]: _hi_0}]);
  } else {
    return run_jump($Map$ins$deep$, [_x_0, _lo_0, _hi_0, _pb_0, _p2_0, run_loop($Map$bit$(_key_0, _p2_0))]);
  }
}

function $Map$diff$fin$(_xt_0, _yt_0, _rc_0) {
  const _r_0 = _rc_0["fst"];
  const _t_0 = _rc_0["snd"];
  if (_t_0) {
    const _x_0 = run_loop($Map$diff$(_xt_0, _yt_0));
    return nat_chk(33n + _x_0);
  } else {
    return _r_0;
  }
}

function $Map$diff$step$(_x_0, _y_0) {
  const _cx_0 = _x_0.codePointAt(0);
  const _cy_0 = _y_0.codePointAt(0);
  return {$: "Tuple", ["fst"]: run_loop($Map$diff$chr$(((_cx_0 ^ _cy_0) >>> 0))), ["snd"]: (_cx_0 === _cy_0)};
}

function $Map$put$bit$(_x_0, _p2_0, _lo_0, _hi_0, _kb_0) {
  const _key2_0 = _kb_0["fst"];
  const _t_0 = _kb_0["snd"];
  if (!_t_0) {
    return {$: "MNode", ["pos"]: _p2_0, ["lo"]: run_loop($Map$put$(_lo_0, _key2_0, _x_0)), ["hi"]: _hi_0};
  } else {
    return {$: "MNode", ["pos"]: _p2_0, ["lo"]: _lo_0, ["hi"]: run_loop($Map$put$(_hi_0, _key2_0, _x_0))};
  }
}

function $Map$bit$go$chr$(_t_0, _r_0) {
  const _c2_0 = _r_0["fst"];
  const _b_0 = _r_0["snd"];
  return {$: "Tuple", ["fst"]: (_c2_0 + _t_0), ["snd"]: _b_0};
}

function $Map$bit$chr$(_c_0, _off_0) {
  const _x_0 = _c_0.codePointAt(0);
  if (_off_0 === 0n) {
    return {$: "Tuple", ["fst"]: char_new(_x_0), ["snd"]: true};
  } else {
    const _b_0 = (_off_0 - 1n);
    return {$: "Tuple", ["fst"]: char_new(_x_0), ["snd"]: run_loop($Map$bit$u$(_x_0, (31n < _b_0 ? 0n : 31n - _b_0)))};
  }
}

function $Map$bit$go$rec$(_c_0, _r_0) {
  const _t2_0 = _r_0["fst"];
  const _b_0 = _r_0["snd"];
  return {$: "Tuple", ["fst"]: (_c_0 + _t2_0), ["snd"]: _b_0};
}

function $vm$register_op$(_group_0, _r_0, _vm_0) {
  if (_group_0 == 0) {
    const _mem_0 = _vm_0["mem"];
    const _ds_0 = _vm_0["ds"];
    const _cs_0 = _vm_0["cs"];
    const _ip_0 = _vm_0["ip"];
    const _control_0 = _vm_0["control"];
    const _output_0 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$read_cell$(_mem_0, _r_0)), run_clo((_x_0) => {
    return run_clo((_x_1) => {
    return {$: "VM", ["mem"]: _x_0, ["ds"]: _ds_0, ["cs"]: {$: "Con", ["head"]: ((_ip_0 + 1) >>> 0), ["tail"]: _cs_0}, ["ip"]: run_loop($vm$jump$(_x_1)), ["control"]: _control_0, ["output"]: _output_0};
});
})]);
  } else if ((_group_0 & 3) == 0) {
    const _111_0 = u32_to_word(_group_0)["tail"]["tail"]["head"];
    const _112_0 = u32_to_word(_group_0)["tail"]["tail"]["tail"];
    const _mem_1 = _vm_0["mem"];
    const _ds_1 = _vm_0["ds"];
    const _cs_1 = _vm_0["cs"];
    const _ip_1 = _vm_0["ip"];
    const _control_1 = _vm_0["control"];
    const _output_1 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_1)), run_clo((_x_2) => {
    return run_clo((_x_3) => {
    return run_jump($vm$pair$, [run_loop($vm$read_cell$(_mem_1, _r_0)), run_clo((_x_4) => {
    return run_clo((_x_5) => {
    const _mem_2 = run_loop($vm$write_cell$(_x_4, _r_0, ((_x_5 + _x_2) >>> 0)));
    return {$: "VM", ["mem"]: _mem_2, ["ds"]: {$: "Con", ["head"]: _x_5, ["tail"]: _x_3}, ["cs"]: _cs_1, ["ip"]: _ip_1, ["control"]: _control_1, ["output"]: _output_1};
});
})]);
});
})]);
  } else if (_group_0 == 2) {
    const _mem_3 = _vm_0["mem"];
    const _ds_2 = _vm_0["ds"];
    const _cs_2 = _vm_0["cs"];
    const _ip_2 = _vm_0["ip"];
    const _control_2 = _vm_0["control"];
    const _output_2 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_2)), run_clo((_x_6) => {
    return run_clo((_x_7) => {
    return {$: "VM", ["mem"]: run_loop($vm$write_cell$(_mem_3, _r_0, _x_6)), ["ds"]: _x_7, ["cs"]: _cs_2, ["ip"]: _ip_2, ["control"]: _control_2, ["output"]: _output_2};
});
})]);
  } else if ((_group_0 & 3) == 2) {
    const _171_0 = u32_to_word(_group_0)["tail"]["tail"]["head"];
    const _172_0 = u32_to_word(_group_0)["tail"]["tail"]["tail"];
    const _mem_4 = _vm_0["mem"];
    const _ds_3 = _vm_0["ds"];
    const _cs_3 = _vm_0["cs"];
    const _ip_3 = _vm_0["ip"];
    const _control_3 = _vm_0["control"];
    const _output_3 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_3)), run_clo((_x_8) => {
    return run_clo((_x_9) => {
    return run_jump($vm$pair$, [run_loop($vm$read_cell$(_mem_4, _r_0)), run_clo((_x_10) => {
    return run_clo((_x_11) => {
    const _mem_5 = run_loop($vm$write_cell$(_x_10, _r_0, ((_x_11 + _x_8) >>> 0)));
    return {$: "VM", ["mem"]: _mem_5, ["ds"]: {$: "Con", ["head"]: _x_11, ["tail"]: _x_9}, ["cs"]: _cs_3, ["ip"]: _ip_3, ["control"]: _control_3, ["output"]: _output_3};
});
})]);
});
})]);
  } else if (_group_0 == 1) {
    const _mem_6 = _vm_0["mem"];
    const _ds_4 = _vm_0["ds"];
    const _cs_4 = _vm_0["cs"];
    const _ip_4 = _vm_0["ip"];
    const _control_4 = _vm_0["control"];
    const _output_4 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$read_cell$(_mem_6, _r_0)), run_clo((_x_12) => {
    return run_clo((_x_13) => {
    return {$: "VM", ["mem"]: _x_12, ["ds"]: {$: "Con", ["head"]: _x_13, ["tail"]: _ds_4}, ["cs"]: _cs_4, ["ip"]: _ip_4, ["control"]: _control_4, ["output"]: _output_4};
});
})]);
  } else {
    const _231_0 = u32_to_word(_group_0)["tail"]["head"];
    const _232_0 = u32_to_word(_group_0)["tail"]["tail"];
    const _mem_7 = _vm_0["mem"];
    const _ds_5 = _vm_0["ds"];
    const _cs_5 = _vm_0["cs"];
    const _ip_5 = _vm_0["ip"];
    const _control_5 = _vm_0["control"];
    const _output_5 = _vm_0["output"];
    return run_jump($vm$pair$, [run_loop($vm$pop$(_ds_5)), run_clo((_x_14) => {
    return run_clo((_x_15) => {
    return run_jump($vm$pair$, [run_loop($vm$read_cell$(_mem_7, _r_0)), run_clo((_x_16) => {
    return run_clo((_x_17) => {
    const _mem_8 = run_loop($vm$write_cell$(_x_16, _r_0, ((_x_17 + _x_14) >>> 0)));
    return {$: "VM", ["mem"]: _mem_8, ["ds"]: {$: "Con", ["head"]: _x_17, ["tail"]: _x_15}, ["cs"]: _cs_5, ["ip"]: _ip_5, ["control"]: _control_5, ["output"]: _output_5};
});
})]);
});
})]);
  }
}

function $vm$shift_valid$(_ok_0, _a_0, _b_0) {
  if (!_ok_0) {
    return 0;
  } else {
    const _x_0 = run_loop($vm$magnitude$(_b_0));
    return run_jump($vm$shift_dir$, [run_loop($vm$negative$(_b_0)), _a_0, BigInt(_x_0)]);
  }
}

function $Map$ins$splice$bit$(_x_0, _rest_0, _pb_0, _kb_0) {
  const _key2_0 = _kb_0["fst"];
  const _t_0 = _kb_0["snd"];
  if (!_t_0) {
    return {$: "MNode", ["pos"]: _pb_0, ["lo"]: {$: "MLeaf", ["key"]: _key2_0, ["val"]: _x_0}, ["hi"]: _rest_0};
  } else {
    return {$: "MNode", ["pos"]: _pb_0, ["lo"]: _rest_0, ["hi"]: {$: "MLeaf", ["key"]: _key2_0, ["val"]: _x_0}};
  }
}

function $Map$ins$deep$(_x_0, _lo_0, _hi_0, _pb_0, _qb_0, _kb_0) {
  const _key2_0 = _kb_0["fst"];
  const _t_0 = _kb_0["snd"];
  if (!_t_0) {
    return {$: "MNode", ["pos"]: _qb_0, ["lo"]: run_loop($Map$ins$(_lo_0, _key2_0, _x_0, _pb_0)), ["hi"]: _hi_0};
  } else {
    return {$: "MNode", ["pos"]: _qb_0, ["lo"]: _lo_0, ["hi"]: run_loop($Map$ins$(_hi_0, _key2_0, _x_0, _pb_0))};
  }
}

function $Map$diff$chr$(_x_0) {
  const _x_1 = run_loop($Map$msb$u$(32n, _x_0));
  return (33n < _x_1 ? 0n : 33n - _x_1);
}

function $Map$bit$u$(_x_0, _k_0) {
  const _x_1 = (_k_0 >= 32n ? 0 : (_x_0 >>> Number(_k_0)) >>> 0);
  const _x_2 = ((_x_1 & 1) >>> 0);
  return (_x_2 !== 0);
}

function $vm$shift_dir$(_right_0, _a_0, _n_0) {
  if (_right_0) {
    return (_n_0 >= 32n ? 0 : (_a_0 >>> Number(_n_0)) >>> 0);
  } else {
    return (_n_0 >= 32n ? 0 : (_a_0 << Number(_n_0)) >>> 0);
  }
}

function $Map$msb$u$(_n_0, _x_0) {
  if (_n_0 === 0n) {
    return 0n;
  } else {
    const _p_0 = (_n_0 - 1n);
    return run_jump($Map$msb$u$if$, [_p_0, _x_0, (_x_0 === 0)]);
  }
}

function $Map$msb$u$if$(_p_0, _x2_0, _z_0) {
  if (_z_0) {
    return 0n;
  } else {
    const _x_0 = run_loop($Map$msb$u$(_p_0, ((_x2_0 >>> 1) >>> 0)));
    return nat_chk(1n + _x_0);
  }
}

function $File$open$(_path_0, _mode_0, _k_0) {
  return { $: "$FFI", run: $0eff.file_open.run, need: $0eff.file_open.need, args: [_path_0, _mode_0], kont: _k_0 };
}

function $File$close$(_file_0, _k_0) {
  return { $: "$FFI", run: $0eff.file_close.run, need: $0eff.file_close.need, args: [_file_0], kont: _k_0 };
}

function $File$read$(_file_0, _max_0, _k_0) {
  return { $: "$FFI", run: $0eff.file_read.run, need: $0eff.file_read.need, args: [_file_0, _max_0], kont: _k_0 };
}

function $IO$write$(_text_0, _k_0) {
  return { $: "$FFI", run: $0eff.io_write.run, need: $0eff.io_write.need, args: [_text_0], kont: _k_0 };
}

// Cli
// ===

// A JS program runs one thread and no GPU: --threads and --gpu do nothing.
let cli_args = [];

function cli(argv) {
  for (let i = 0; i < argv.length; i += 1) {
    if (argv[i] === "--") {
      cli_args.push(...argv.slice(i + 1));
      break;
    } else if (argv[i] === "--help") {
      io_out(1, io_bytes("usage: " + process.argv[1] + "\n"));
      process.exit(0);
    } else if (argv[i] === "--threads" || argv[i] === "--gpu") {
      i += 1;
    } else {
      cli_args.push(argv[i]);
    }
  }
}

// Show
// ====

// char_show: an escape, a \u{hex}, else the code point
function show_chr(c, q) {
  const k = { 10: "n", 9: "t", 13: "r", 0: "0", 92: "\\" }[c]
    ?? (c === q.codePointAt(0) ? q : null);
  return k !== null ? "\\" + k : c < 32 || c === 127
    ? "\\u{" + c.toString(16) + "}" : String.fromCodePoint(c);
}

// A pure main's value as term_show spells it (see show_main); chain is the
// bracket it continues, or 0.
function show_val(D, N, d, v, chain) {
  if (D[d] === 7) {
    const fs = Object.values(typeof v === "boolean"
      ? { $: v ? "True" : "False" } : v);
    let a = d + 3;
    for (; N[D[a]] !== fs[0]; a += 4 + 2 * D[a + 2]) {}
    const o = "{[("[D[a + 3]];
    let s = o === "{" ? fs[0] + "{" : chain === o ? "" : o;
    for (const [j, f] of fs.slice(1).entries()) {
      if (o === "[" ? j === 0 && chain === o : j > 0) {
        s += ", ";
      }
      s += show_val(D, N, D[a + 5 + 2 * j], f, j === 1 && o !== "{" ? o : 0);
    }
    return o === "{" || chain !== o ? s + "}])"[D[a + 3]] : s;
  }
  return D[d] === 0 ? String(v)
    : D[d] === 1 ? f32_show(v).replace(/^-?\d+(?=e|$)/, "$&.0")
    : D[d] === 2 ? v + "n"
    : D[d] === 3 ? "'" + show_chr(v.codePointAt(0), "'") + "'"
    : D[d] === 4 ? "\"" + [...v].map((c) =>
      show_chr(c.codePointAt(0), "\"")).join("") + "\""
    : D[d] === 5 ? "{==}"
    : "[" + v.map((x) => show_val(D, N, D[d + 1], x, 0)).join(", ") + "]";
}

// Io
// ==

function io_exit(main, show) {
  try {
    if (show !== null) {
      io_out(1, io_bytes(show_val(...show, 0, run_loop(main()), 0) + "\n"));
      process.exit(0);
    }
    process.exit(io_run(main));
  } catch (e) {
    io_errs(String(e));
    process.exit(1);
  }
}

function io_out(fd, data) {
  const fs = require("fs");
  let at = 0;
  while (at < data.length) {
    try {
      at += fs.writeSync(fd, data, at, data.length - at);
    } catch (e) {
      if (e.code === "EAGAIN" || e.code === "EINTR") {
        continue;
      }
      try {
        fs.writeSync(2, "bend: a short write on a standard stream\n");
      } catch (o) {
      }
      process.exit(1);
    }
  }
}

function io_errs(message) {
  io_out(2, io_bytes(message + "\n"));
}

function io_sys() {
  if (globalThis.BEND_SYS === undefined) {
    const ffi = require("bun:ffi");
    const mac = process.platform === "darwin";
    const err = mac ? "__error" : "__errno_location";
    // Darwin's extended select supports high fds.
    const sel = mac ? "select$DARWIN_EXTSN" : "select";
    const T = { i: "i32", u: "u32", U: "u64", I: "i64", p: "ptr",
      c: "cstring" };
    // Apple arm64 passes variadic fcntl flags on the stack: use the ninth
    // fixed argument (the third elsewhere).
    const vari = mac && process.arch === "arm64";
    const lib = ffi.dlopen(mac ? "libSystem.dylib" : "libc.so.6",
      Object.fromEntries(("socket:iii>i bind:ipu>i listen:ii>i connect:ipu>i"
        + " accept:ipp>i send:ipUi>I recv:ipUi>I read:ipU>I pread:ipUI>I"
        + " sendto:ipUipu>I recvfrom:ipUipp>I close:i>i setsockopt:iiipu>i"
        + " " + sel + ":ipppp>i"
        + (vari ? " fcntl:iiiiiiiii>i" : " fcntl:iii>i") + " getsockopt:iiipp>i"
        + " strerror:i>c " + err + ":>p").split(" ").map((s) => {
        const [name, args, ret] = s.split(/[:>]/);
        return [name, { args: [...args].map((a) => T[a]), returns: T[ret] }];
      }))).symbols;
    const fcntl = (fd, cmd, arg) => vari
      ? lib.fcntl(fd, cmd, 0, 0, 0, 0, 0, 0, arg)
      : lib.fcntl(fd, cmd, arg);
    globalThis.BEND_SYS = { ...lib, fcntl, select: lib[sel],
      ptr: ffi.ptr, mac,
      errno: () => ffi.read.i32(lib[err](), 0) };
  }
  return globalThis.BEND_SYS;
}

function io_fail(code) {
  return { $: "Fail",
    error: io_tup(code >>> 0, String(io_sys().strerror(code))) };
}

function io_done(value) {
  return { $: "Done", value };
}

function io_tup(...xs) {
  return xs.reduceRight((snd, fst) => ({ $: "Tuple", fst, snd }));
}

function io_bytes(text) {
  return new TextEncoder().encode(text);
}

function io_text(b, n) {
  return new TextDecoder("utf-8", { ignoreBOM: true }).decode(b.subarray(0, n));
}

function io_addr(host, port) {
  const part = host.split(".");
  const deci = (p) => /^(0|[1-9]\d{0,2})$/.test(p) && Number(p) < 256;
  if (port > 65535 || part.length !== 4 || !part.every(deci)) {
    return null;
  }
  const b = new Uint8Array(16);
  const head = io_sys().mac ? [16, 2] : [2, 0];
  b.set([...head, port >> 8, port & 255, ...part.map(Number)]);
  return b;
}

function io_push(fun, arg, fresh) {
  const io = globalThis.BEND_IO;
  io.runs.push({ fun, arg });
  io.live += fresh ? 1 : 0;
}

function io_wait(io) {
  const soon = io.waits.reduce((m, w) => Math.min(m, w.at ?? m), Infinity);
  const ms = soon === Infinity ? -1
    : Math.max(0, Math.ceil(soon - performance.now()));
  const fds = io.waits.filter((w) => w.fd !== undefined);
  const top = fds.reduce((m, w) => Math.max(m, w.fd), 0);
  const len = (top >> 6 << 3) + 8;
  const set = new Uint8Array(2 * len);
  const at = (w) => (w.out ? len : 0) + (w.fd >> 3);
  for (const w of fds) {
    set[at(w)] |= 1 << (w.fd & 7);
  }
  const tv = new BigInt64Array([BigInt(ms / 1000 | 0),
    BigInt(ms % 1000 * 1000)]);
  const sys = io_sys();
  sys.select(top + 1, sys.ptr(set), sys.ptr(set, len), null,
    ms < 0 ? null : sys.ptr(tv));
  const now = performance.now();
  io.waits = io.waits.filter((w) => {
    const ready = w.at <= now || w.fd !== undefined
      && set[at(w)] & 1 << (w.fd & 7);
    if (ready) {
      io_push(io_wake, w, false);
    }
    return !ready;
  });
}

// Resume k with more's value; undefined means re-parked.
function io_wake(w) {
  const x = w.more();
  return x === undefined ? undefined : w.k(x);
}

// Park for read/write (out) or until at (performance.now()); an undefined
// fd or at disables that source.
function io_park_on(fd, out, k, more, at) {
  globalThis.BEND_IO.waits.push({ fd, out, k, more, at });
}

function io_run(m) {
  const io = { runs: [], live: 0, waits: [] };
  globalThis.BEND_IO = io;
  try {
    io_push(run_loop(m()), (x) => ({ $: "Emit", value: x }), true);
    for (;;) {
      if (io.runs.length === 0) {
        if (io.live === 0) {
          return 0;
        }
        if (io.waits.length === 0) {
          io_errs("bend: deadlock: every computation waits on a channel");
          return 1;
        }
        io_wait(io);
        continue;
      }
      const s = io.runs.shift();
      let op = s.fun(s.arg);
      while (op !== undefined) {
        if (op.$ === "Emit") {
          io.live -= 1;
          break;
        }
        if (op.$ === "Halt") {
          io_errs(op.message);
          return op.code;
        }
        const need = op.need?.() ?? {};
        if (need.time || need.read) {
          const more = () => op.run(...op.args, op.kont);
          io_park_on(need.read ? op.args[0] : undefined, false, op.kont, more,
            need.read ? undefined : performance.now() + Number(op.args[0]));
          break;
        }
        const x = op.run(...op.args, op.kont);
        if (x === undefined) {
          break;
        }
        op = op.kont(x);
      }
    }
  } catch (req) {
    if (req instanceof RangeError) {
      throw "bend: memory fault (machine stack overflow?)";
    }
    if (req?.$ !== "$FFI") {
      throw req;
    }
    io_errs("bend: runtime fail-stop");
    return 1;
  }
}

cli(process.argv.slice(2));
io_exit($main$, null);