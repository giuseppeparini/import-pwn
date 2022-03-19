function _G.exit()
  vim.cmd('quitall')
end

function _G.error(...)
  io.stderr.write(...)
  exit()
end

function _G.assert(cond, ...)
  if not cond then
    _G.error(...)
  end
  return cond
end

-- number in LuaJIT is f64 so it can store an i53
local ffi = require('ffi')
ffi.cdef([[
typedef union {
  uint8_t u8[8];
  int64_t i64;
} I64;
typedef union {
  uint8_t u8[4];
  int32_t i32;
} I32;
I64 atoll(char *string);
I32 atoi(char *string);
]])

local I64 = {}

function I64:parse(str)
  local c_str = ffi.new('char[?]', #str + 1)
  ffi.copy(c_str, str)
  local o = { inner = ffi.C.atoll(c_str) }
  setmetatable(o, self)
  self.__index = self
  return o
end

function I64:bswap()
  for i = 0, 3 do
    local tmp = self.inner.u8[i]
    self.inner.u8[i] = self.inner.u8[7 - i]
    self.inner.u8[7 - i] = tmp
  end
end

function I64:tobuffer()
  local buf = ''
  for i = 0, 7 do
    buf = buf .. string.char(self.inner.u8[i])
  end
  return buf
end

function I64:__tostring()
  return tostring(self.inner.i64)
end

local I32 = {}

function I32:parse(str)
  local c_str = ffi.new('char[?]', #str + 1)
  ffi.copy(c_str, str)
  local o = { inner = ffi.C.atoi(c_str) }
  setmetatable(o, self)
  self.__index = self
  return o
end

function I32:bswap()
  for i = 0, 1 do
    local tmp = self.inner.u8[i]
    self.inner.u8[i] = self.inner.u8[3 - i]
    self.inner.u8[3 - i] = tmp
  end
end

function I32:tobuffer()
  local buf = ''
  for i = 0, 3 do
    buf = buf .. string.char(self.inner.u8[i])
  end
  return buf
end

function I32:__tostring()
  return tostring(self.inner.i32)
end

local LineStream = {}

function LineStream:new(inner)
  local o = { inner = inner, buf = '' }
  setmetatable(o, self)
  self.__index = self
  return o
end

function LineStream:buffer_read_line()
  local i, j = string.find(self.buf, '\n')
  if i and j then
    local res = string.sub(self.buf, 1, i - 1)
    self.buf = string.sub(self.buf, j + 1)
    return res
  end
  return nil
end

function LineStream:read_line(cb)
  if not self.inner then
    return cb(nil)
  end

  local res = self:buffer_read_line()
  if res then
    return cb(res)
  end

  vim.loop.read_start(self.inner, function(err, data)
    if err then
      vim.defer_fn(function()
        error(err)
      end, 0)
      return
    end

    if data then
      self.buf = self.buf .. data
    else
      local res = self.buf
      vim.loop.shutdown(self.inner)
      vim.loop.close(self.inner)
      self.buf = nil
      self.inner = nil

      if #res == 0 then
        res = nil
      end
      return vim.defer_fn(function()
        cb(res)
      end, 0)
    end

    res = self:buffer_read_line()
    if res then
      vim.loop.read_stop(self.inner)
      return vim.defer_fn(function()
        cb(res)
      end, 0)
    end
  end)
end

local function resolve_host(hostname, port)
  local ok, err = vim.loop.getaddrinfo(
    hostname,
    tostring(port),
    { family = 'inet', socktype = 'stream', protocol = 'tcp' }
  )
  assert(ok, err)
  local res = ok[1]
  assert(res, 'Cannot resolve hostname ' .. hostname)
  return res
end

local function connect(client, hostname, port, cb)
  local info = resolve_host(hostname, port)
  return assert(pcall(function()
    return client:connect(info.addr, info.port, function(...)
      local args = { ... }
      vim.defer_fn(function()
        cb(unpack(args))
      end, 0)
    end)
  end))
end

local function solve_loop(client, reader)
  reader:read_line(function(line)
    if not line then
      exit()
    end

    io.write(line .. '\n')
    local res = nil

    local n, bits, endian = line:match(
      '^Please send me the number (%d+) as a (%d+)-bit (%a+)-endian '
    )

    if
      n
      and (bits == '32' or bits == '64')
      and (endian == 'big' or endian == 'little')
    then
      local class = bits == '32' and I32 or I64
      n = class.parse(class, n)
      if (ffi.abi('le') and 'little' or 'big') ~= endian then
        n:bswap()
      end
      res = n:tobuffer()
    elseif line:find('Please send me an empty line ', 1, true) == 1 then
      res = '\n'
    end

    if not res then
      error('unreachable')
    end

    vim.loop.write(client, res, function(err)
      if err then
        return vim.defer_fn(function()
          error(err)
        end, 0)
      end

      reader:read_line(function(line)
        vim.defer_fn(function()
          assert(line, 'unreachable')

          io.write(line .. '\n')

          solve_loop(client, reader)
        end, 0)
      end)
    end)
  end)
end

local client = vim.loop.new_tcp()
connect(client, 'piecewise.challs.cyberchallenge.it', 9110, function(err)
  assert(not err, err)
  local reader = LineStream:new(client)
  solve_loop(client, reader)
end)
