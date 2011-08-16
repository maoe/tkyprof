// Quoted by "JavsScrpt: The Good Parts"

Function.prototype.method = function(name, func) {
  if (!this.prototype[name]) {
    this.prototype[name] = func;
    return this;
  }
}

Function.method('curry', function() {
  var slice = Array.prototype.slice,
      args = slice.apply(arguments),
      that = this;
  return function() {
    return that.apply(null, args.concat(slice.apply(arguments)));
  };
});

Array.method('shift', function() {
  return this.splice(0, 1)[0];
});

Array.method('splice', function(start, deleteCount) {
  var max = Math.max,
      min = Math.min,
      delta,
      element,
      insertCount = max(arguments.length - 2, 0),
      k = 0,
      len = this.length,
      newLen,
      result = [],
      shiftCount;

  start = start || 0;
  if (start < 0) {
    start += len;
  }
  start = max(min(start, len), 0);
  deleteCount = max(
    min(
      typeof deleteCount === 'number'
        ? deleteCount
        : len,
      len - start
    ),
    0
  );
  delta = insertCount - deleteCount;
  newLen = len + delta;
  while (k < deleteCount) {
    element = this[start + k];
    if (element !== undefined) {
      result[k] = element;
    }
    k += 1;
  }
  shiftCount = len - start - deleteCount;
  if (delta < 0) {
    k = start + insertCount;
    while (shiftCount) {
      this[k] = this[k - delta];
      k += 1;
      shiftCount -= 1;
    }
    this.length = newLen;
  } else if (delta > 0) {
    k = 1;
    while (shiftCount) {
      this[newLen - k] = this[len - k];
      k += 1;
      shiftCount -= 1;
    }
  }
  for (k = 0; k < insertCount; k += 1) {
    this[start + k] = arguments[k + 2];
  }
  return result;
});

Array.method('unshift', function() {
  this.splice.apply(
    this,
    [0, 0].concat(Array.prototype.slice.apply(arguments))
  );
  return this.length;
});
