// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   If the non-updatable flag is undefined, the thunk is updatable.
*/
function T(f, nu) {
    this.f = f;
    if(nu === undefined) {
        this.x = __updatable;
    }
}

function F(f) {
    this.f = f;
}

// Special object used for blackholing.
var __blackhole = {};

// Used to indicate that an object is updatable.
var __updatable = {};

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        f = B(f);
        if(!(f instanceof Function)) {
            return f;
        }
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f !== __blackhole) {
            var f = t.f;
            t.f = __blackhole;
            if(t.x === __updatable) {
                t.x = f();
            } else {
                return f();
            }
        }
        return t.x;
    } else {
        return t;
    }
}

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    while(f instanceof F) {
        var fun = f.f;
        f.f = __blackhole;
        f = fun();
    }
    return f;
}

// Export Haste, A, B and E. Haste because we need to preserve exports, A, B
// and E because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;
window['B'] = B;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
if(Math.imul) {
    var imul = Math.imul;
} else {
    var imul = function(a, b) {
        // ignore high a * high a as the result will always be truncated
        var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
        var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
        var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
        return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
    }
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = B(A(f, [[0, str.charCodeAt(i)], acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = B(A(f, [mv.x]));
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['v']['i8'][0] = 'U'.charCodeAt(0);
    le['v']['i8'][1] = 'T'.charCodeAt(0);
    le['v']['i8'][2] = 'F'.charCodeAt(0);
    le['v']['i8'][3] = '-'.charCodeAt(0);
    le['v']['i8'][4] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return B(A(act,[0]));
    } catch(e) {
        return B(A(handler,[e, 0]));
    }
}

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round;
var jsTrunc = Math.trunc ? Math.trunc : function(x) {
    return x < 0 ? Math.ceil(x) : Math.floor(x);
};
function jsRoundW(n) {
    return Math.abs(jsTrunc(n));
}
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.currentTarget.offsetLeft || 0),
	    posy - (e.currentTarget.offsetTop || 0)];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                B(A(cb,[[0,k.keyCode],0]));
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,x.button],[0,mx,my],0]));
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,mx,my],0]));
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {B(A(cb,[[0,x.keyCode],0]));};
        break;
    case 'wheel':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            var mdx = [0,x.deltaX];
            var mdy = [0,x.deltaY];
            var mdz = [0,x.deltaZ];
            B(A(cb,[[0,mx,my],[0,mdx,mdy,mdz],0]));
        };
        break;
    default:
        fun = function() {B(A(cb,[0]));};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {B(A(cb,[0]));}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsElemsByClassName(cls) {
    var es = document.getElementsByClassName(cls);
    var els = [0];

    for (var i = es.length-1; i >= 0; --i) {
        els = [1, [0, es[i]], els];
    }
    return els;
}

function jsQuerySelectorAll(elem, query) {
    var els = [0], nl;

    if (!elem || typeof elem.querySelectorAll !== 'function') {
        return els;
    }

    nl = elem.querySelectorAll(query);

    for (var i = nl.length-1; i >= 0; --i) {
        els = [1, [0, nl[i]], els];
    }

    return els;
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetFirstChild(elem) {
    var len = elem.childNodes.length;
    for(var i = 0; i < len; i++) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

var jsJSONParse = JSON.parse;

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, jsRead(obj)];
    case 'string':
        return [1, obj];
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else if (obj == null) {
            return [5];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);}),true]
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem], new T(function() {return arr2lst(arr,elem+1);})]
}
window['arr2lst'] = arr2lst;

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}
window['lst2arr'] = lst2arr;

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);

    if(method == "POST") {
        xhr.setRequestHeader("Content-type",
                             "application/x-www-form-urlencoded");
    }
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                B(A(cb,[[1,[0,xhr.responseText]],0]));
            } else {
                B(A(cb,[[0],0])); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {return x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
    var a = x[0], b = x[1], c = x[2], d = x[3];

    a = ff(a, b, c, d, k[0], 7, -680876936);
    d = ff(d, a, b, c, k[1], 12, -389564586);
    c = ff(c, d, a, b, k[2], 17,  606105819);
    b = ff(b, c, d, a, k[3], 22, -1044525330);
    a = ff(a, b, c, d, k[4], 7, -176418897);
    d = ff(d, a, b, c, k[5], 12,  1200080426);
    c = ff(c, d, a, b, k[6], 17, -1473231341);
    b = ff(b, c, d, a, k[7], 22, -45705983);
    a = ff(a, b, c, d, k[8], 7,  1770035416);
    d = ff(d, a, b, c, k[9], 12, -1958414417);
    c = ff(c, d, a, b, k[10], 17, -42063);
    b = ff(b, c, d, a, k[11], 22, -1990404162);
    a = ff(a, b, c, d, k[12], 7,  1804603682);
    d = ff(d, a, b, c, k[13], 12, -40341101);
    c = ff(c, d, a, b, k[14], 17, -1502002290);
    b = ff(b, c, d, a, k[15], 22,  1236535329);

    a = gg(a, b, c, d, k[1], 5, -165796510);
    d = gg(d, a, b, c, k[6], 9, -1069501632);
    c = gg(c, d, a, b, k[11], 14,  643717713);
    b = gg(b, c, d, a, k[0], 20, -373897302);
    a = gg(a, b, c, d, k[5], 5, -701558691);
    d = gg(d, a, b, c, k[10], 9,  38016083);
    c = gg(c, d, a, b, k[15], 14, -660478335);
    b = gg(b, c, d, a, k[4], 20, -405537848);
    a = gg(a, b, c, d, k[9], 5,  568446438);
    d = gg(d, a, b, c, k[14], 9, -1019803690);
    c = gg(c, d, a, b, k[3], 14, -187363961);
    b = gg(b, c, d, a, k[8], 20,  1163531501);
    a = gg(a, b, c, d, k[13], 5, -1444681467);
    d = gg(d, a, b, c, k[2], 9, -51403784);
    c = gg(c, d, a, b, k[7], 14,  1735328473);
    b = gg(b, c, d, a, k[12], 20, -1926607734);

    a = hh(a, b, c, d, k[5], 4, -378558);
    d = hh(d, a, b, c, k[8], 11, -2022574463);
    c = hh(c, d, a, b, k[11], 16,  1839030562);
    b = hh(b, c, d, a, k[14], 23, -35309556);
    a = hh(a, b, c, d, k[1], 4, -1530992060);
    d = hh(d, a, b, c, k[4], 11,  1272893353);
    c = hh(c, d, a, b, k[7], 16, -155497632);
    b = hh(b, c, d, a, k[10], 23, -1094730640);
    a = hh(a, b, c, d, k[13], 4,  681279174);
    d = hh(d, a, b, c, k[0], 11, -358537222);
    c = hh(c, d, a, b, k[3], 16, -722521979);
    b = hh(b, c, d, a, k[6], 23,  76029189);
    a = hh(a, b, c, d, k[9], 4, -640364487);
    d = hh(d, a, b, c, k[12], 11, -421815835);
    c = hh(c, d, a, b, k[15], 16,  530742520);
    b = hh(b, c, d, a, k[2], 23, -995338651);

    a = ii(a, b, c, d, k[0], 6, -198630844);
    d = ii(d, a, b, c, k[7], 10,  1126891415);
    c = ii(c, d, a, b, k[14], 15, -1416354905);
    b = ii(b, c, d, a, k[5], 21, -57434055);
    a = ii(a, b, c, d, k[12], 6,  1700485571);
    d = ii(d, a, b, c, k[3], 10, -1894986606);
    c = ii(c, d, a, b, k[10], 15, -1051523);
    b = ii(b, c, d, a, k[1], 21, -2054922799);
    a = ii(a, b, c, d, k[8], 6,  1873313359);
    d = ii(d, a, b, c, k[15], 10, -30611744);
    c = ii(c, d, a, b, k[6], 15, -1560198380);
    b = ii(b, c, d, a, k[13], 21,  1309151649);
    a = ii(a, b, c, d, k[4], 6, -145523070);
    d = ii(d, a, b, c, k[11], 10, -1120210379);
    c = ii(c, d, a, b, k[2], 15,  718787259);
    b = ii(b, c, d, a, k[9], 21, -343485551);

    x[0] = add32(a, x[0]);
    x[1] = add32(b, x[1]);
    x[2] = add32(c, x[2]);
    x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
    a = add32(add32(a, q), add32(x, t));
    return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
    return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
    return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
    return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
    return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
    var n = s.length,
        state = [1732584193, -271733879, -1732584194, 271733878], i;
    for (i=64; i<=s.length; i+=64) {
        md5cycle(state, md5blk(s.substring(i-64, i)));
    }
    s = s.substring(i-64);
    var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
    for (i=0; i<s.length; i++)
        tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
    tail[i>>2] |= 0x80 << ((i%4) << 3);
    if (i > 55) {
        md5cycle(state, tail);
        for (i=0; i<16; i++) tail[i] = 0;
    }
    tail[14] = n*8;
    md5cycle(state, tail);
    return state;
}
window['md51'] = md51;

function md5blk(s) {
    var md5blks = [], i;
    for (i=0; i<64; i+=4) {
        md5blks[i>>2] = s.charCodeAt(i)
            + (s.charCodeAt(i+1) << 8)
            + (s.charCodeAt(i+2) << 16)
            + (s.charCodeAt(i+3) << 24);
    }
    return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
    var s='', j=0;
    for(; j<4; j++)
        s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
        + hex_chr[(n >> (j * 8)) & 0x0F];
    return s;
}

function hex(x) {
    for (var i=0; i<x.length; i++)
        x[i] = rhex(x[i]);
    return x.join('');
}

function md5(s) {
    return hex(md51(s));
}

function add32(a, b) {
    return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

// "Weak Pointers". Mostly useless implementation since
// JS does its own GC.

function mkWeak(key, val, fin) {
    fin = !fin? function() {}: fin;
    return {key: key, val: val, fin: fin};
}

function derefWeak(w) {
    return [0, 1, E(w).val];
}

function finalizeWeak(w) {
    return [0, B(A(E(w).fin, [0]))];
}

var _0=0,_1=function(_2,_3){return new F(function(){return A(_3,[_2]);});},_4=function(_5){var _6=jsTrunc(_5),_7=_6;return [0,_7>>>0&255];},_8=new T(function(){return [0,"(function(b,i){return b.getUint8(i);})"];}),_9=function(_a){var _b=B(A(_a,[_])),_c=_b;return E(_c);},_d=function(_e){return new F(function(){return _9(function(_){var _=0;return new F(function(){return eval(E(_e)[1]);});});});},_f=new T(function(){return B(_d(_8));}),_g=function(_h){return function(_i,_){var _j=B(A(new T(function(){return B(A(_f,[E(_h)]));}),[E(E(_i)[1]),_])),_k=_j;return new T(function(){return B(_4(_k));});};},_l=function(_m){var _n=jsTrunc(_m),_o=_n;return [0,_o];},_p=new T(function(){return [0,"(function(b,i){return b.getInt32(i,true);})"];}),_q=new T(function(){return B(_d(_p));}),_r=function(_s){return function(_t,_){var _u=B(A(new T(function(){return B(A(_q,[E(_s)]));}),[E(E(_t)[1]),_])),_v=_u;return new T(function(){return B(_l(_v));});};},_w=function(_x,_y){return [1,[0,new T(function(){return [0,E(_y)[1]+4|0];}),new T(function(){return [0,B(_9(function(_){var _=0;return new F(function(){return A(_r,[_x,_y,_]);});}))[1]];})]];},_z=function(_A,_B){var _C=E(_A);return _C[0]==0?E(_B):[1,_C[1],new T(function(){return B(_z(_C[2],_B));})];},_D=function(_E,_F){var _G=jsShowI(_E),_H=_G;return new F(function(){return _z(fromJSStr(_H),_F);});},_I=[0,41],_J=[0,40],_K=function(_L,_M,_N){return _M>=0?B(_D(_M,_N)):_L<=6?B(_D(_M,_N)):[1,_J,new T(function(){var _O=jsShowI(_M),_P=_O;return B(_z(fromJSStr(_P),[1,_I,_N]));})];},_Q=[0],_R=function(_S){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_K(9,_S,_Q));}))));});},_T=function(_U){var _V=E(_U)[1];if(_V>>>0>1114111){return new F(function(){return _R(_V);});}else{return [0,_V];}},_W=function(_X,_Y){var _Z=B(_w(_X,_Y));return _Z[0]==0?[0,_Z[1]]:[1,new T(function(){var _10=E(_Z[1]);return [0,_10[1],new T(function(){return B(_T(_10[2]));})];})];},_11=[0,_W],_12=new T(function(){return [0,"(function(a,x) {a.push(x);})"];}),_13=new T(function(){return B(_d(_12));}),_14=function(_15,_16){return [0,_15,_16];},_17=function(_18){return E(_18);},_19=new T(function(){return B(_14(_17,_17));}),_1a=[0,4],_1b=new T(function(){return [0,"Int32Array"];}),_1c=function(_1d){return E(E(_1d)[2]);},_1e=new T(function(){return [0,"window[\'toABle\']"];}),_1f=new T(function(){return B(_d(_1e));}),_1g=function(_1h,_1i){return function(_1j){return function(_1k,_){return new F(function(){return A(new T(function(){return B(A(new T(function(){return B(A(_1f,[E(E(_1i)[1])]));}),[E(E(_1j)[1])]));}),[B(A(new T(function(){return B(_1c(_1h));}),[_1k])),_]);});};};},_1l=function(_1m){return E(E(_1m)[1]);},_1n=function(_1o){return function(_1p,_){var _1q=B(A(_13,[E(_1p),E(new T(function(){return B(_9(function(_){var _=0;return new F(function(){return A(_1g,[_19,_1b,_1a,new T(function(){return B(_1l(_1o));}),_]);});}));})),_])),_1r=_1q;return _0;};},_1s=function(_1t){return [0,B(_1n(new T(function(){return [0,E(_1t)[1]];})))];},_1u=[0,_11,_1s],_1v=function(_1w,_1x){return [1,[0,_1x,_Q]];},_1y=function(_1z){return I_toInt(_1z)>>>0;},_1A=function(_1B){var _1C=E(_1B);return _1C[0]==0?_1C[1]>>>0:B(_1y(_1C[1]));},_1D=function(_1E){return [0,B(_1A(_1E))];},_1F=function(_1G,_1H){return [1,new T(function(){return B(_1D(_1G));}),_1H];},_1I=[0,1],_1J=function(_1K){return [0,_1K];},_1L=function(_1M){return [1,I_fromInt(_1M)];},_1N=function(_1O){var _1P=_1O&4294967295;return _1P<0?B(_1L(_1O)):B(_1J(_1P));},_1Q=[0,0],_1R=function(_1S,_1T){var _1U=E(_1S);if(!_1U[0]){var _1V=_1U[1],_1W=E(_1T);return _1W[0]==0?_1V>=_1W[1]:I_compareInt(_1W[1],_1V)<=0;}else{var _1X=_1U[1],_1Y=E(_1T);return _1Y[0]==0?I_compareInt(_1X,_1Y[1])>=0:I_compare(_1X,_1Y[1])>=0;}},_1Z=function(_20,_21){var _22=E(_20);if(!_22[0]){var _23=_22[1],_24=E(_21);return _24[0]==0?_23>_24[1]:I_compareInt(_24[1],_23)<0;}else{var _25=_22[1],_26=E(_21);return _26[0]==0?I_compareInt(_25,_26[1])>0:I_compare(_25,_26[1])>0;}},_27=function(_28,_29){var _2a=E(_28);if(!_2a[0]){var _2b=_2a[1],_2c=E(_29);return _2c[0]==0?_2b<_2c[1]:I_compareInt(_2c[1],_2b)>0;}else{var _2d=_2a[1],_2e=E(_29);return _2e[0]==0?I_compareInt(_2d,_2e[1])<0:I_compare(_2d,_2e[1])<0;}},_2f=function(_2g,_2h){while(1){var _2i=E(_2g);if(!_2i[0]){var _2j=_2i[1],_2k=E(_2h);if(!_2k[0]){var _2l=_2k[1],_2m=addC(_2j,_2l);if(!E(_2m[2])){return [0,_2m[1]];}else{_2g=[1,I_fromInt(_2j)];_2h=[1,I_fromInt(_2l)];continue;}}else{_2g=[1,I_fromInt(_2j)];_2h=_2k;continue;}}else{var _2n=E(_2h);if(!_2n[0]){_2g=_2i;_2h=[1,I_fromInt(_2n[1])];continue;}else{return [1,I_add(_2i[1],_2n[1])];}}}},_2o=function(_2p,_2q,_2r,_2s,_2t){if(!B(_1R(_2s,_1Q))){var _2u=function(_2v){if(!B(_27(_2v,_2t))){return new F(function(){return A(_2p,[_2v,new T(function(){return B(_2u(B(_2f(_2v,_2s))));})]);});}else{return E(_2q);}};return new F(function(){return _2u(_2r);});}else{var _2w=function(_2x){if(!B(_1Z(_2x,_2t))){return new F(function(){return A(_2p,[_2x,new T(function(){return B(_2w(B(_2f(_2x,_2s))));})]);});}else{return E(_2q);}};return new F(function(){return _2w(_2r);});}},_2y=function(_2z,_2A){return new F(function(){return _2o(_1F,_Q,B(_1N(_2z)),_1I,B(_1N(_2A)));});},_2B=function(_2C){return E(E(_2C)[1]);},_2D=function(_2E){var _2F=jsTrunc(_2E),_2G=_2F;return [0,_2G>>>0];},_2H=new T(function(){return [0,"(function(b,i){return b.getUint32(i,true);})"];}),_2I=new T(function(){return B(_d(_2H));}),_2J=function(_2K){return function(_2L,_){var _2M=B(A(new T(function(){return B(A(_2I,[E(_2K)]));}),[E(E(_2L)[1]),_])),_2N=_2M;return new T(function(){return B(_2D(_2N));});};},_2O=function(_2P){return function(_2Q,_2R){var _2S=B(_2y(1,B(_9(function(_){var _=0;return new F(function(){return A(_2J,[_2Q,_2R,_]);});}))[1]));if(!_2S[0]){return [1,[0,new T(function(){return [0,E(_2R)[1]+4|0];}),_Q]];}else{var _2T=E(new T(function(){return B(_2B(_2P));}))[1],_2U=B(A(_2T,[_2Q,new T(function(){return [0,E(_2R)[1]+4|0];})]));if(!_2U[0]){return [0,_2U[1]];}else{var _2V=E(_2U[1]),_2W=function(_2X){var _2Y=E(_2X);return _2Y[0]==0?_1v:function(_2Z,_30){var _31=B(A(_2T,[_2Z,_30]));if(!_31[0]){return [0,_31[1]];}else{var _32=E(_31[1]),_33=B(A(E(new T(function(){return [0,B(_2W(_2Y[2]))];}))[1],[_2Z,_32[1]]));if(!_33[0]){return E(_33);}else{var _34=E(_33[1]);return [1,[0,_34[1],[1,_32[2],_34[2]]]];}}};},_35=B(A(B(_2W(_2S[2])),[_2Q,_2V[1]]));if(!_35[0]){return E(_35);}else{var _36=E(_35[1]);return [1,[0,_36[1],[1,_2V[2],_36[2]]]];}}}};},_37=new T(function(){return [0,function(_38,_39){var _3a=B(A(B(_2O(_1u)),[_38,_39]));return _3a[0]==0?[0,_3a[1]]:[1,new T(function(){var _3b=E(_3a[1]);return [0,_3b[1],[0,_3b[2]]];})];}];}),_3c=new T(function(){return B(unCStr("Wrong magic byte for ServerException"));}),_3d=[0,_3c],_3e=function(_3f,_3g){if(B(_9(function(_){var _=0;return new F(function(){return A(_g,[_3f,_3g,_]);});}))[1]!=2){return E(_3d);}else{return new F(function(){return A(E(_37)[1],[_3f,new T(function(){return [0,E(_3g)[1]+1|0];})]);});}},_3h=[0,_3e],_3i=function(_3j,_3k){return [0,E(_3j)[1]+E(_3k)[1]|0];},_3l=function(_3m){var _3n=jsTrunc(_3m),_3o=_3n;return [0,_3o];},_3p=new T(function(){return [0,"(function(b){return b.size;})"];}),_3q=new T(function(){return B(_d(_3p));}),_3r=function(_3s){return new F(function(){return _9(function(_){var _=0,_3t=B(A(_3q,[E(_3s),_])),_3u=_3t;return new T(function(){return B(_3l(_3u));});});});},_3v=new T(function(){return [0,"(function(b){try {return new Blob([b]);} catch (e) {return new Blob([b.buffer]);}})"];}),_3w=new T(function(){return B(_d(_3v));}),_3x=[0,0],_3y=new T(function(){return [0,"(function(b,off,len){return b.slice(off,len);})"];}),_3z=function(_3A,_3B,_3C){var _3D=new T(function(){return B(_9(function(_){var _=0;return new F(function(){return A(_3w,[E(_3C),_]);});}));}),_3E=E(_3A);if(!_3E){var _3F=E(_3B);if(B(_3r(_3D))[1]<=_3F[1]){return E(_3D);}else{return new F(function(){return _9(function(_){var _=0;return new F(function(){return A(_d,[_3y,E(_3D),E(E(_3x)[1]),E(E(_3F)[1]),_]);});});});}}else{return new F(function(){return _9(function(_){var _=0;return new F(function(){return A(_d,[_3y,E(_3D),E(_3E),E(_3E+E(_3B)[1]|0),_]);});});});}},_3G=function(_3H,_3I){var _3J=B(_w(_3H,_3I));if(!_3J[0]){return [0,_3J[1]];}else{var _3K=E(_3J[1]),_3L=_3K[1],_3M=_3K[2];return [1,[0,new T(function(){return B(_3i(_3L,_3M));}),new T(function(){return B(_3z(E(_3L)[1],_3M,_3H));})]];}},_3N=[0,_3G],_3O=new T(function(){return B(unCStr("Wrong magic byte for ServerReply"));}),_3P=[0,_3O],_3Q=function(_3R,_3S){if(B(_9(function(_){var _=0;return new F(function(){return A(_g,[_3R,_3S,_]);});}))[1]!=1){return E(_3P);}else{var _3T=new T(function(){return [0,E(_3S)[1]+1|0];}),_3U=B(A(E(_3N)[1],[_3R,new T(function(){return [0,E(_3T)[1]+4|0];})]));if(!_3U[0]){return [0,_3U[1]];}else{var _3V=E(_3U[1]);return [1,[0,_3V[1],[0,new T(function(){return [0,B(_9(function(_){var _=0;return new F(function(){return A(_r,[_3R,_3T,_]);});}))[1]];}),_3V[2]]]];}}},_3W=[0,_3Q],_3X=[0,0],_3Y=new T(function(){return [0,"(function(b,cb){var r=new FileReader();r.onload=function(){B(A(cb,[new DataView(r.result),0]));};r.readAsArrayBuffer(b);})"];}),_3Z=new T(function(){return B(_d(_3Y));}),_40=function(_41){return [2];},_42=function(_43,_){while(1){var _44=E(_43);if(!_44[0]){return _0;}else{var _45=_44[2],_46=E(_44[1]);switch(_46[0]){case 0:var _47=B(A(_46[1],[_])),_48=_47;_43=B(_z(_45,[1,_48,_Q]));continue;case 1:_43=B(_z(_45,_46[1]));continue;default:_43=_45;continue;}}}},_49=[2],_4a=function(_4b){return new F(function(){return _40(_4b);});},_4c=function(_4d,_4e,_4f){return [0,function(_){var _4g=E(_4d)[1],_4h=rMV(_4g),_4i=_4h,_4j=E(_4i);if(!_4j[0]){var _=wMV(_4g,[0,_4j[1],new T(function(){return B(_z(_4j[2],[1,[0,_4e,function(_4k){return E(new T(function(){return B(A(_4f,[_0]));}));}],_Q]));})]);return _49;}else{var _4l=E(_4j[1]);if(!_4l[0]){var _=wMV(_4g,[0,_4e,_Q]);return new T(function(){return B(A(_4f,[_0]));});}else{var _=wMV(_4g,[1,_4l[2]]);return [1,[1,new T(function(){return B(A(_4f,[_0]));}),[1,new T(function(){return B(A(_4l[1],[_4e,_4a]));}),_Q]]];}}}];},_4m=[1,_Q],_4n=function(_4o,_4p){return [0,function(_){var _4q=E(_4o)[1],_4r=rMV(_4q),_4s=_4r,_4t=E(_4s);if(!_4t[0]){var _4u=_4t[1],_4v=E(_4t[2]);if(!_4v[0]){var _=wMV(_4q,_4m);return new T(function(){return B(A(_4p,[_4u]));});}else{var _4w=E(_4v[1]),_=wMV(_4q,[0,_4w[1],_4v[2]]);return [1,[1,new T(function(){return B(A(_4p,[_4u]));}),[1,new T(function(){return B(A(_4w[2],[_4a]));}),_Q]]];}}else{var _=wMV(_4q,[1,new T(function(){return B(_z(_4t[1],[1,function(_4x){return function(_4y){return E(new T(function(){return B(A(_4p,[_4x]));}));};},_Q]));})]);return _49;}}];},_4z=function(_4A){return function(_4B){return [0,function(_){var _4C=nMV(_4m),_4D=_4C,_4E=[0,_4D];return [0,function(_){var _4F=B(A(_3Z,[E(_4A),function(_4G,_){return new F(function(){return _42([1,new T(function(){return B(_4c(_4E,[0,_3X,new T(function(){return B(_3r(_4A));}),_4G],_40));}),_Q],_);});},_])),_4H=_4F;return new T(function(){return B(_4n(_4E,_4B));});}];}];};},_4I=function(_4J,_4K){var _4L=E(_4K);if(!_4L[0]){return [0,_Q,_Q];}else{var _4M=_4L[1];if(!B(A(_4J,[_4M]))){return [0,_Q,_4L];}else{var _4N=new T(function(){var _4O=B(_4I(_4J,_4L[2]));return [0,_4O[1],_4O[2]];});return [0,[1,_4M,new T(function(){return E(E(_4N)[1]);})],new T(function(){return E(E(_4N)[2]);})];}}},_4P=function(_4Q){return new F(function(){return A(_4Q,[_0]);});},_4R=new T(function(){return B(unCStr("WebSockets connection died for some reason!"));}),_4S=new T(function(){return B(err(_4R));}),_4T=[1,_0],_4U=new T(function(){return B(unCStr("ServerException"));}),_4V=new T(function(){return B(unCStr("Haste.App.Protocol"));}),_4W=new T(function(){return B(unCStr("haste-lib-0.4.4.3"));}),_4X=new T(function(){var _4Y=hs_wordToWord64(2073178634),_4Z=_4Y,_50=hs_wordToWord64(3222190412),_51=_50;return [0,_4Z,_51,[0,_4Z,_51,_4W,_4V,_4U],_Q];}),_52=function(_53){return E(_4X);},_54=function(_55){return E(E(_55)[1]);},_56=new T(function(){return B(unCStr("Maybe.fromJust: Nothing"));}),_57=new T(function(){return B(err(_56));}),_58=function(_59,_5a,_5b){var _5c=new T(function(){var _5d=B(A(_59,[_5b])),_5e=B(A(_5a,[new T(function(){var _5f=E(_5c);return _5f[0]==0?E(_57):E(_5f[1]);})])),_5g=hs_eqWord64(_5d[1],_5e[1]),_5h=_5g;if(!E(_5h)){var _5i=[0];}else{var _5j=hs_eqWord64(_5d[2],_5e[2]),_5k=_5j,_5i=E(_5k)==0?[0]:[1,_5b];}var _5l=_5i,_5m=_5l;return _5m;});return E(_5c);},_5n=function(_5o){var _5p=E(_5o);return new F(function(){return _58(B(_54(_5p[1])),_52,_5p[2]);});},_5q=[0,34],_5r=new T(function(){return B(unCStr("ServerException "));}),_5s=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_5t=new T(function(){return B(err(_5s));}),_5u=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_5v=new T(function(){return B(err(_5u));}),_5w=function(_5x,_5y){while(1){var _5z=E(_5x);if(!_5z[0]){return E(_5v);}else{var _5A=E(_5y);if(!_5A){return E(_5z[1]);}else{_5x=_5z[2];_5y=_5A-1|0;continue;}}}},_5B=new T(function(){return B(unCStr("ACK"));}),_5C=new T(function(){return B(unCStr("BEL"));}),_5D=new T(function(){return B(unCStr("BS"));}),_5E=new T(function(){return B(unCStr("SP"));}),_5F=[1,_5E,_Q],_5G=new T(function(){return B(unCStr("US"));}),_5H=[1,_5G,_5F],_5I=new T(function(){return B(unCStr("RS"));}),_5J=[1,_5I,_5H],_5K=new T(function(){return B(unCStr("GS"));}),_5L=[1,_5K,_5J],_5M=new T(function(){return B(unCStr("FS"));}),_5N=[1,_5M,_5L],_5O=new T(function(){return B(unCStr("ESC"));}),_5P=[1,_5O,_5N],_5Q=new T(function(){return B(unCStr("SUB"));}),_5R=[1,_5Q,_5P],_5S=new T(function(){return B(unCStr("EM"));}),_5T=[1,_5S,_5R],_5U=new T(function(){return B(unCStr("CAN"));}),_5V=[1,_5U,_5T],_5W=new T(function(){return B(unCStr("ETB"));}),_5X=[1,_5W,_5V],_5Y=new T(function(){return B(unCStr("SYN"));}),_5Z=[1,_5Y,_5X],_60=new T(function(){return B(unCStr("NAK"));}),_61=[1,_60,_5Z],_62=new T(function(){return B(unCStr("DC4"));}),_63=[1,_62,_61],_64=new T(function(){return B(unCStr("DC3"));}),_65=[1,_64,_63],_66=new T(function(){return B(unCStr("DC2"));}),_67=[1,_66,_65],_68=new T(function(){return B(unCStr("DC1"));}),_69=[1,_68,_67],_6a=new T(function(){return B(unCStr("DLE"));}),_6b=[1,_6a,_69],_6c=new T(function(){return B(unCStr("SI"));}),_6d=[1,_6c,_6b],_6e=new T(function(){return B(unCStr("SO"));}),_6f=[1,_6e,_6d],_6g=new T(function(){return B(unCStr("CR"));}),_6h=[1,_6g,_6f],_6i=new T(function(){return B(unCStr("FF"));}),_6j=[1,_6i,_6h],_6k=new T(function(){return B(unCStr("VT"));}),_6l=[1,_6k,_6j],_6m=new T(function(){return B(unCStr("LF"));}),_6n=[1,_6m,_6l],_6o=new T(function(){return B(unCStr("HT"));}),_6p=[1,_6o,_6n],_6q=[1,_5D,_6p],_6r=[1,_5C,_6q],_6s=[1,_5B,_6r],_6t=new T(function(){return B(unCStr("ENQ"));}),_6u=[1,_6t,_6s],_6v=new T(function(){return B(unCStr("EOT"));}),_6w=[1,_6v,_6u],_6x=new T(function(){return B(unCStr("ETX"));}),_6y=[1,_6x,_6w],_6z=new T(function(){return B(unCStr("STX"));}),_6A=[1,_6z,_6y],_6B=new T(function(){return B(unCStr("SOH"));}),_6C=[1,_6B,_6A],_6D=new T(function(){return B(unCStr("NUL"));}),_6E=[1,_6D,_6C],_6F=[0,92],_6G=new T(function(){return B(unCStr("\\DEL"));}),_6H=new T(function(){return B(unCStr("\\a"));}),_6I=new T(function(){return B(unCStr("\\\\"));}),_6J=new T(function(){return B(unCStr("\\SO"));}),_6K=new T(function(){return B(unCStr("\\r"));}),_6L=new T(function(){return B(unCStr("\\f"));}),_6M=new T(function(){return B(unCStr("\\v"));}),_6N=new T(function(){return B(unCStr("\\n"));}),_6O=new T(function(){return B(unCStr("\\t"));}),_6P=new T(function(){return B(unCStr("\\b"));}),_6Q=function(_6R,_6S){if(_6R<=127){var _6T=E(_6R);switch(_6T){case 92:return new F(function(){return _z(_6I,_6S);});break;case 127:return new F(function(){return _z(_6G,_6S);});break;default:if(_6T<32){var _6U=E(_6T);switch(_6U){case 7:return new F(function(){return _z(_6H,_6S);});break;case 8:return new F(function(){return _z(_6P,_6S);});break;case 9:return new F(function(){return _z(_6O,_6S);});break;case 10:return new F(function(){return _z(_6N,_6S);});break;case 11:return new F(function(){return _z(_6M,_6S);});break;case 12:return new F(function(){return _z(_6L,_6S);});break;case 13:return new F(function(){return _z(_6K,_6S);});break;case 14:return new F(function(){return _z(_6J,new T(function(){var _6V=E(_6S);if(!_6V[0]){var _6W=[0];}else{var _6W=E(E(_6V[1])[1])==72?B(unAppCStr("\\&",_6V)):E(_6V);}return _6W;}));});break;default:return new F(function(){return _z([1,_6F,new T(function(){var _6X=_6U;return _6X>=0?B(_5w(_6E,_6X)):E(_5t);})],_6S);});}}else{return [1,[0,_6T],_6S];}}}else{return [1,_6F,new T(function(){var _6Y=jsShowI(_6R),_6Z=_6Y;return B(_z(fromJSStr(_6Z),new T(function(){var _70=E(_6S);if(!_70[0]){var _71=[0];}else{var _72=E(_70[1])[1],_71=_72<48?E(_70):_72>57?E(_70):B(unAppCStr("\\&",_70));}return _71;})));})];}},_73=new T(function(){return B(unCStr("\\\""));}),_74=function(_75,_76){var _77=E(_75);if(!_77[0]){return E(_76);}else{var _78=_77[2],_79=E(E(_77[1])[1]);if(_79==34){return new F(function(){return _z(_73,new T(function(){return B(_74(_78,_76));}));});}else{return new F(function(){return _6Q(_79,new T(function(){return B(_74(_78,_76));}));});}}},_7a=function(_7b,_7c,_7d){return _7b<11?B(_z(_5r,[1,_5q,new T(function(){return B(_74(_7c,[1,_5q,_7d]));})])):[1,_J,new T(function(){return B(_z(_5r,[1,_5q,new T(function(){return B(_74(_7c,[1,_5q,[1,_I,_7d]]));})]));})];},_7e=function(_7f){return new F(function(){return _7a(0,E(_7f)[1],_Q);});},_7g=function(_7h,_7i){return new F(function(){return _7a(0,E(_7h)[1],_7i);});},_7j=[0,44],_7k=[0,93],_7l=[0,91],_7m=function(_7n,_7o,_7p){var _7q=E(_7o);return _7q[0]==0?B(unAppCStr("[]",_7p)):[1,_7l,new T(function(){return B(A(_7n,[_7q[1],new T(function(){var _7r=function(_7s){var _7t=E(_7s);return _7t[0]==0?E([1,_7k,_7p]):[1,_7j,new T(function(){return B(A(_7n,[_7t[1],new T(function(){return B(_7r(_7t[2]));})]));})];};return B(_7r(_7q[2]));})]));})];},_7u=function(_7v,_7w){return new F(function(){return _7m(_7g,_7v,_7w);});},_7x=function(_7y,_7z,_7A){return new F(function(){return _7a(E(_7y)[1],E(_7z)[1],_7A);});},_7B=[0,_7x,_7e,_7u],_7C=new T(function(){return [0,_52,_7B,_7D,_5n];}),_7D=function(_7w){return [0,_7C,_7w];},_7E=function(_7F,_7G){return new F(function(){return die(new T(function(){return B(A(_7G,[_7F]));}));});},_7H=function(_7I){return new F(function(){return _7E(_7I,_7D);});},_7J=[0,0],_7K=function(_7L,_7M){return E(_7L)[1]!=E(_7M)[1];},_7N=new T(function(){return B(unCStr("Not enough data!"));}),_7O=[0,_7N],_7P=new T(function(){return [0,"(function(url, cb, f, err) {var ws = new WebSocket(url);ws.binaryType = \'blob\';ws.onmessage = function(e) {B(A(cb,[ws,e.data,0]));};ws.onopen = function(e) {B(A(f,[ws,0]));};ws.onerror = function(e) {B(A(err,[0]));};return ws;})"];}),_7Q=new T(function(){return B(_d(_7P));}),_7R=function(_7S,_7T,_7U,_7V,_7W){return [0,function(_){var _7X=nMV(_4m),_7Y=_7X,_7Z=[0,_7Y],_80=function(_81){return new F(function(){return _4c(_7Z,_81,_40);});};return [0,function(_){var _82=B(A(_7Q,[E(toJSStr(E(_7S))),function(_83,_84,_){return new F(function(){return _42([1,new T(function(){return B(A(_7T,[_83,_84,_40]));}),_Q],_);});},function(_85,_){return new F(function(){return _42([1,new T(function(){return B(A(_7V,[_85,_80]));}),_Q],_);});},function(_){return new F(function(){return _42([1,new T(function(){return B(A(_7U,[_80]));}),_Q],_);});},_])),_86=_82;return new T(function(){return B(_4n(_7Z,_7W));});}];}];},_87=new T(function(){return [0,"(function(s, msg) {s.send(msg);})"];}),_88=new T(function(){return B(_d(_87));}),_89=function(_8a,_8b,_8c,_8d,_){return [0,function(_){return new F(function(){return _42([1,[0,function(_){var _8e=nMV(_Q),_8f=_8e;return [0,function(_){var _8g=nMV(_7J),_8h=_8g;return [0,function(_){var _8i=nMV([0,function(_8j,_8k,_8l){var _8m=new T(function(){return E(E(_8k)[1]);});return new F(function(){return _4n(_8m,function(_8n){return E(new T(function(){return B(_7R(new T(function(){return E(E(_8b)[1]);}),function(_8o,_8p){return new F(function(){return (function(_8q,_8r){return new F(function(){return (function(_8s){return function(_8t){return new F(function(){return A(new T(function(){return B(_4z(_8s));}),[function(_8u){return [0,function(_){var _8v=mMV(_8f,function(_8w){if(!E(new T(function(){var _8x=E(_8u),_8y=B(A(E(_3h)[1],[_8x[3],_8x[1]]));if(!_8y[0]){var _8z=E(_4T);}else{var _8A=E(_8y[1]),_8z=E(_8A[1])[1]>E(_8x[2])[1]?E(_4T):B(_7H(_8A[2]));}var _8B=_8z,_8C=_8B;return _8C;}))[0]){return [0,_8w,_4P];}else{var _8D=E(new T(function(){var _8E=E(_8u),_8F=B(A(E(_3W)[1],[_8E[3],_8E[1]]));if(!_8F[0]){var _8G=[0,_8F[1]];}else{var _8H=E(_8F[1]),_8G=E(_8H[1])[1]>E(_8E[2])[1]?E(_7O):[1,_8H[2]];}var _8I=_8G,_8J=_8I;return _8J;}));if(!_8D[0]){return [0,_8w,_4P];}else{var _8K=E(_8D[1]),_8L=B(_4I(function(_8M){return new F(function(){return _7K(E(_8M)[1],_8K[1]);});},_8w)),_8N=E(_8L[2]);return _8N[0]==0?[0,_8w,_4P]:[0,new T(function(){return B(_z(_8L[1],_8N[2]));}),function(_8p){return new F(function(){return _4c(E(_8N[1])[2],_8K[2],_8p);});}];}}}),_8O=_8v;return new T(function(){return B(A(_8O,[_8t]));});}];}]);});};})(_8r);});})(_8o,_8p);});},_4S,_1,function(_8P){var _8Q=new T(function(){return B(A(_88,[E(_8P)]));});return new F(function(){return _4c(_8m,function(_8R,_8S,_8T){return [0,function(_){var _8U=B(A(_8Q,[E(_8R),_])),_8V=_8U;return new T(function(){return B(A(_8T,[_0]));});}];},function(_8W){return E([0,function(_){var _8X=B(A(_8Q,[E(_8j),_])),_8Y=_8X;return new T(function(){return B(A(_8l,[_0]));});}]);});});}));}));});});},_Q]),_8Z=_8i;return new T(function(){return B(A(_8a,[[0,[0,_8Z],[0,_8h],[0,_8f]],_40]));});}];}];}],_Q],_);});},_8c,_8d,_8b];},_90=[0,2],_91=function(_92,_93){return [1,[0,_93,_0]];},_94=[0,_91],_95=function(_96,_){return _0;},_97=function(_98,_){return new F(function(){return _95(_98,_);});},_99=[0,_97],_9a=function(_9b){return E(_99);},_9c=[0,_94,_9a],_9d=function(_9e){return [0,function(_9f,_){var _9g=B(A(B(_1n(new T(function(){return [0,B(_3r(_9e))[1]];}))),[_9f,_])),_9h=_9g,_9i=B(A(_13,[E(_9f),E(_9e),_])),_9j=_9i;return _0;}];},_9k=[0,_3N,_9d],_9l=[0,0],_9m=[0,1],_9n=new T(function(){return [0,"Uint8Array"];}),_9o=function(_9p){return function(_9q,_){var _9r=B(A(_13,[E(_9q),E(new T(function(){return B(_9(function(_){var _=0;return new F(function(){return A(_1g,[_19,_9n,_9m,new T(function(){return B(_1l(_9p));}),_]);});}));})),_])),_9s=_9r;return _0;};},_9t=new T(function(){return [0,B(_9o(_9l))];}),_9u=function(_9v,_9w){while(1){var _9x=E(_9v);if(!_9x[0]){return E(_9w);}else{_9v=_9x[2];var _9y=_9w+1|0;_9w=_9y;continue;}}},_9z=new T(function(){return [0,"Uint32Array"];}),_9A=function(_9B){return function(_9C,_){var _9D=B(A(_13,[E(_9C),E(new T(function(){return B(_9(function(_){var _=0;return new F(function(){return A(_1g,[_19,_9z,_1a,new T(function(){return B(_1l(_9B));}),_]);});}));})),_])),_9E=_9D;return _0;};},_9F=function(_9G){return E(E(_9G)[2]);},_9H=function(_9I,_9J){return function(_9K,_){var _9L=B(A(B(_9A(new T(function(){return [0,B(_9u(_9J,0))>>>0];}))),[_9K,_])),_9M=_9L;return new F(function(){return A(E(new T(function(){var _9N=function(_9O){var _9P=E(_9O);return _9P[0]==0?_95:function(_9Q,_){var _9R=B(A(B(A(new T(function(){return B(_9F(_9I));}),[_9P[1]]))[1],[_9Q,_])),_9S=_9R;return new F(function(){return A(E(new T(function(){return [0,B(_9N(_9P[2]))];}))[1],[_9Q,_]);});};};return [0,B(_9N(_9J))];}))[1],[_9K,_]);});};},_9T=function(_9U,_9V,_9W){return function(_9X,_){var _9Y=B(A(E(_9t)[1],[_9X,_])),_9Z=_9Y,_a0=B(A(E(new T(function(){return [0,B(_1n(new T(function(){return [0,E(_9U)[1]];})))];}))[1],[_9X,_])),_a1=_a0,_a2=B(A(E(new T(function(){return [0,B(_1n(new T(function(){return [0,E(_9V)[1]];})))];}))[1],[_9X,_])),_a3=_a2;return new F(function(){return A(E(new T(function(){return [0,B(_9H(_9k,_9W))];}))[1],[_9X,_]);});};},_a4=function(_a5){return [0,new T(function(){return [0,E(_a5)[1]+1|0];}),_a5];},_a6=new T(function(){return B(unCStr("Unable to decode return value!"));}),_a7=new T(function(){return B(err(_a6));}),_a8=function(_a9,_aa){while(1){var _ab=E(_a9);if(!_ab[0]){return E(_aa);}else{_a9=_ab[2];var _ac=[1,_ab[1],_aa];_aa=_ac;continue;}}},_ad=new T(function(){return [0,"(function(){return [];})"];}),_ae=new T(function(){return [0,"(function(parts){return new Blob(parts);})"];}),_af=new T(function(){return B(_d(_ae));}),_ag=function(_ah){return new F(function(){return _9(function(_){var _=0,_ai=B(A(_d,[_ad,_])),_aj=_ai,_ak=B(A(_ah,[_aj,_])),_al=_ak;return new F(function(){return A(_af,[E(_aj),_]);});});});},_am=function(_an,_ao,_ap){return function(_aq,_ar){var _as=new T(function(){return E(E(_aq)[1]);});return new F(function(){return _4n(_as,function(_at){return new F(function(){return _4c(_as,_at,function(_au){return E([0,function(_){var _av=nMV(_4m),_aw=_av,_ax=[0,_aw];return [0,function(_){var _ay=E(_aq),_az=mMV(E(_ay[2])[1],_a4),_aA=_az;return [0,function(_){var _aB=mMV(E(_ay[3])[1],function(_aC){return [0,[1,[0,_aA,_ax],_aC],_0];}),_aD=_aB;return new T(function(){return B(A(_at,[new T(function(){return B(_ag(B(_9T(_aA,_ao,new T(function(){return B(_a8(_ap,_Q));})))));}),_ay,function(_aE){return E(new T(function(){return B(_4n(_ax,function(_aF){return new F(function(){return A(_4z,[_aF,function(_aG){var _aH=E(_aG),_aI=B(A(E(new T(function(){return B(_2B(_an));}))[1],[_aH[3],_aH[1]]));if(!_aI[0]){return E(_a7);}else{var _aJ=E(_aI[1]);return E(_aJ[1])[1]>E(_aH[2])[1]?E(_a7):B(A(_ar,[_aJ[2]]));}}]);});}));}));}]));});}];}];}]);});});});});};},_aK=[0,0],_aL=new T(function(){return B(_am(_9c,_aK,_Q));}),_aM=new T(function(){return [0,B(_2O(_1u))];}),_aN=function(_aO,_aP){return [1,[0,new T(function(){return [0,E(_aP)[1]+4|0];}),new T(function(){return [0,B(_9(function(_){var _=0;return new F(function(){return A(_2J,[_aO,_aP,_]);});}))[1]];})]];},_aQ=function(_aR){var _aS=jsTrunc(_aR),_aT=_aS;return [0,_aT>>>0&65535];},_aU=new T(function(){return [0,"(function(b,i){return b.getUint16(i,true);})"];}),_aV=new T(function(){return B(_d(_aU));}),_aW=function(_aX){return function(_aY,_){var _aZ=B(A(new T(function(){return B(A(_aV,[E(_aX)]));}),[E(E(_aY)[1]),_])),_b0=_aZ;return new T(function(){return B(_aQ(_b0));});};},_b1=function(_b2,_b3){var _b4=B(_aN(_b2,_b3));if(!_b4[0]){return [0,_b4[1]];}else{var _b5=E(_b4[1]),_b6=_b5[1],_b7=new T(function(){return [0,E(_b6)[1]+2|0];}),_b8=new T(function(){return [0,E(_b7)[1]+4|0];}),_b9=new T(function(){return B(_aW(_b2));});return [1,[0,new T(function(){return [0,E(_b8)[1]+2|0];}),[0,_b5[2],new T(function(){return B(_9(function(_){var _=0;return new F(function(){return A(_b9,[_b6,_]);});}));}),new T(function(){return [0,B(_9(function(_){var _=0;return new F(function(){return A(_2J,[_b2,_b7,_]);});}))[1]];}),new T(function(){return B(_9(function(_){var _=0;return new F(function(){return A(_b9,[_b8,_]);});}));})]]];}},_ba=function(_bb,_bc){var _bd=B(_b1(_bb,_bc));if(!_bd[0]){return [0,_bd[1]];}else{var _be=E(_bd[1]),_bf=_be[1];return [1,[0,new T(function(){return [0,E(_bf)[1]+1|0];}),[0,_be[2],new T(function(){return B(_9(function(_){var _=0;return new F(function(){return A(_g,[_bb,_bf,_]);});}));})]]];}},_bg=new T(function(){return [0,"(function(b,i){return b.getFloat64(i,true);})"];}),_bh=new T(function(){return B(_d(_bg));}),_bi=function(_bj){return function(_bk,_){var _bl=B(A(new T(function(){return B(A(_bh,[E(_bj)]));}),[E(E(_bk)[1]),_])),_bm=_bl;return [0,_bm];};},_bn=function(_bo,_bp){var _bq=B(_ba(_bo,_bp));if(!_bq[0]){return [0,_bq[1]];}else{var _br=E(_bq[1]),_bs=E(_aM)[1],_bt=B(A(_bs,[_bo,_br[1]]));if(!_bt[0]){return [0,_bt[1]];}else{var _bu=E(_bt[1]),_bv=_bu[1],_bw=new T(function(){return [0,E(_bv)[1]+8|0];}),_bx=B(A(_bs,[_bo,new T(function(){return [0,E(_bw)[1]+8|0];})]));if(!_bx[0]){return [0,_bx[1]];}else{var _by=E(_bx[1]),_bz=new T(function(){return B(_bi(_bo));});return [1,[0,_by[1],[0,_br[2],_bu[2],new T(function(){return B(_9(function(_){var _=0;return new F(function(){return A(_bz,[_bv,_]);});}));}),new T(function(){return B(_9(function(_){var _=0;return new F(function(){return A(_bz,[_bw,_]);});}));}),_by[2]]]];}}}},_bA=[0,_bn],_bB=new T(function(){return B(unCStr("Control.Exception.Base"));}),_bC=new T(function(){return B(unCStr("base"));}),_bD=new T(function(){return B(unCStr("PatternMatchFail"));}),_bE=new T(function(){var _bF=hs_wordToWord64(18445595),_bG=_bF,_bH=hs_wordToWord64(52003073),_bI=_bH;return [0,_bG,_bI,[0,_bG,_bI,_bC,_bB,_bD],_Q];}),_bJ=function(_bK){return E(_bE);},_bL=function(_bM){var _bN=E(_bM);return new F(function(){return _58(B(_54(_bN[1])),_bJ,_bN[2]);});},_bO=function(_bP){return E(E(_bP)[1]);},_bQ=function(_bR,_bS){return new F(function(){return _z(E(_bR)[1],_bS);});},_bT=function(_bU,_bV){return new F(function(){return _7m(_bQ,_bU,_bV);});},_bW=function(_bX,_bY,_bZ){return new F(function(){return _z(E(_bY)[1],_bZ);});},_c0=[0,_bW,_bO,_bT],_c1=new T(function(){return [0,_bJ,_c0,_c2,_bL];}),_c2=function(_c3){return [0,_c1,_c3];},_c4=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_c5=[0,32],_c6=[0,10],_c7=[1,_c6,_Q],_c8=function(_c9){return E(E(_c9)[1])==124?false:true;},_ca=function(_cb,_cc){var _cd=B(_4I(_c8,B(unCStr(_cb)))),_ce=_cd[1],_cf=function(_cg,_ch){return new F(function(){return _z(_cg,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_z(_cc,new T(function(){return B(_z(_ch,_c7));})));})));}));});},_ci=E(_cd[2]);if(!_ci[0]){return new F(function(){return _cf(_ce,_Q);});}else{return E(E(_ci[1])[1])==124?B(_cf(_ce,[1,_c5,_ci[2]])):B(_cf(_ce,_Q));}},_cj=function(_ck){return new F(function(){return _7E([0,new T(function(){return B(_ca(_ck,_c4));})],_c2);});},_cl=new T(function(){return B(_cj("Packet.hs:84:3-46|function put"));}),_cm=[0,2],_cn=new T(function(){return [0,"Uint16Array"];}),_co=function(_cp){return function(_cq,_){var _cr=B(A(_13,[E(_cq),E(new T(function(){return B(_9(function(_){var _=0;return new F(function(){return A(_1g,[_19,_cn,_cm,new T(function(){return B(_1l(_cp));}),_]);});}));})),_])),_cs=_cr;return _0;};},_ct=function(_cu,_cv,_cw,_cx){return function(_cy,_){var _cz=B(A(B(_9A([0,_cu])),[_cy,_])),_cA=_cz,_cB=B(A(E(new T(function(){return [0,B(_co(_cv))];}))[1],[_cy,_])),_cC=_cB,_cD=B(A(E(new T(function(){return [0,B(_9A([0,E(_cw)[1]]))];}))[1],[_cy,_])),_cE=_cD;return new F(function(){return A(E(new T(function(){return [0,B(_co(_cx))];}))[1],[_cy,_]);});};},_cF=function(_cG){var _cH=E(_cG);if(!_cH[0]){var _cI=E(_cH[1]);return function(_cJ,_){var _cK=B(A(B(_ct(E(_cI[1])[1],_cI[2],_cI[3],_cI[4])),[_cJ,_])),_cL=_cK;return new F(function(){return A(E(new T(function(){return [0,B(_9o(_cH[2]))];}))[1],[_cJ,_]);});};}else{return E(_cl);}},_cM=new T(function(){return [0,"(function(f) {var a=new ArrayBuffer(8);new DataView(a).setFloat64(0,f,true);return a;})"];}),_cN=new T(function(){return B(_d(_cM));}),_cO=function(_cP,_){return new F(function(){return A(_cN,[E(E(_cP)[1]),_]);});},_cQ=function(_cR,_){return new F(function(){return _cO(_cR,_);});},_cS=function(_cT){return function(_cU,_){var _cV=B(A(_13,[E(_cU),E(new T(function(){return B(_9(function(_){var _=0;return new F(function(){return _cQ(_cT,_);});}));})),_])),_cW=_cV;return _0;};},_cX=function(_cY,_cZ,_d0,_d1,_d2){return function(_d3,_){var _d4=B(A(B(_cF(_cY)),[_d3,_])),_d5=_d4,_d6=B(A(E(new T(function(){return [0,B(_9H(_1u,_cZ))];}))[1],[_d3,_])),_d7=_d6,_d8=B(A(E(new T(function(){return [0,B(_cS(_d0))];}))[1],[_d3,_])),_d9=_d8,_da=B(A(E(new T(function(){return [0,B(_cS(_d1))];}))[1],[_d3,_])),_db=_da;return new F(function(){return A(E(new T(function(){return [0,B(_9H(_1u,_d2))];}))[1],[_d3,_]);});};},_dc=function(_dd){var _de=E(_dd);return [0,B(_cX(_de[1],_de[2],_de[3],_de[4],_de[5]))];},_df=[0,_bA,_dc],_dg=[0,46],_dh=[1,_dg,_Q],_di=function(_dj){var _dk=E(_dj);if(!_dk[0]){return [0];}else{return new F(function(){return _z(_dk[1],new T(function(){return B(_di(_dk[2]));}));});}},_dl=function(_dm,_dn){var _do=E(_dn);return _do[0]==0?[0]:[1,_dm,[1,_do[1],new T(function(){return B(_dl(_dm,_do[2]));})]];},_dp=function(_dq){return new F(function(){return _di([1,new T(function(){return B(_K(0,_dq&255&4294967295,_Q));}),new T(function(){return B(_dl(_dh,[1,new T(function(){return B(_K(0,_dq>>>8&255&4294967295,_Q));}),[1,new T(function(){return B(_K(0,_dq>>>16&255&4294967295,_Q));}),[1,new T(function(){return B(_K(0,_dq>>>24&255&4294967295,_Q));}),_Q]]]));})]);});},_dr=function(_ds){return new F(function(){return err(_ds);});},_dt=new T(function(){return B(unCStr("Pattern match failure in do expression at GeoSniff.hs:168:5-72"));}),_du=new T(function(){return B(_dr(_dt));}),_dv=function(_dw){return function(_dx){return function(_dy){var _dz=new T(function(){return B(A(new T(function(){return B(_dv(_dw));}),[_dx,_dy]));});return new F(function(){return A(new T(function(){return B(A(new T(function(){var _dA=E(E(_dw)[2]);return B(_am(_df,_dA[1],_dA[2]));}),[_dx]));}),[function(_dB){var _dC=E(_dB),_dD=_dC[2],_dE=E(_dC[1]);if(!_dE[0]){var _dF=E(_dE[1]),_dG=E(_dE[2])[1];return (_dG&2)>>>0!=0?(_dG&16)>>>0!=0?[0,function(_){var _dH=placeMarker_ffi(toJSStr(E(_dD)),toJSStr(B(_dp(E(_dF[3])[1]))),E(_dF[4])[1]&4294967295,toJSStr(E(_dC[5])),E(_dF[2])[1]&4294967295,E(_dC[3])[1],E(_dC[4])[1]);return _dz;}]:[0,function(_){var _dI=removeMarker_ffi(toJSStr(E(_dD)));return _dz;}]:[0,function(_){var _dJ=removeMarker_ffi(toJSStr(E(_dD)));return _dz;}];}else{return E(_du);}}]);});};};},_dK=[0,1],_dL=[0,_dK,_Q],_dM=[0,_aK,_Q],_dN=[0,_dM,_dL],_dO=new T(function(){return B(_dv(_dN));}),_dP=function(_dQ){return function(_dR){return new F(function(){return A(new T(function(){return B(A(_aL,[_dQ]));}),[function(_dS){return E([1,[1,new T(function(){return B(A(_dR,[_0]));}),[1,new T(function(){return B(A(_dO,[_dQ,_4a]));}),_Q]]]);}]);});};},_dT=function(_dU,_dV){return [0,_dU,_dV,_Q];},_dW=new T(function(){return B(unCStr("runApp is single-entry!"));}),_dX=new T(function(){return B(err(_dW));}),_dY=true,_dZ=function(_e0){return [0,_dY,_e0];},_e1=false,_e2=function(_){var _=0,_e3=nMV(_e1),_e4=_e3;return [0,_e4];},_e5=new T(function(){return B(_9(_e2));}),_e6=new T(function(){return B(unCStr("Prelude.undefined"));}),_e7=new T(function(){return B(err(_e6));}),_e8=function(_e9,_ea,_){var _eb=mMV(E(_e5)[1],_dZ),_ec=_eb;if(!E(_ec)){var _ed=B(_89(_dP,new T(function(){return B(_dT(_e9,_ea));}),_90,_e7,_)),_ee=_ed;return new F(function(){return A(E(_ee)[1],[_]);});}else{return E(_dX);}},_ef=[0,0],_eg=function(_eh){return function(_ei){return new F(function(){return _e8(new T(function(){return B(unAppCStr("ws://",new T(function(){return fromJSStr(E(_eh)[1]);})));}),_ef,_ei);});};},_ej=[0,_eg],_ek=function(_){var _el=setStartClientCallback_ffi(E(_ej)[1]);return _0;},_em=function(_){return new F(function(){return _ek(_);});};
var hasteMain = function() {B(A(_em, [0]));};window.onload = hasteMain;