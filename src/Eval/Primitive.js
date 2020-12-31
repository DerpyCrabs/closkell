const $$sum = (a, b) => a + b
const $$sub = (a, b) => a - b
const $$prod = (a, b) => a * b
const $$div = (a, b) => a / b
const $$rem = (a, b) => a % b
const $$num_eq = (a, b) => a === b
const $$num_not_eq = (a, b) => a !== b
const $$lt = (a, b) => a < b
const $$gt = (a, b) => a > b
const $$lte = (a, b) => a <= b
const $$gte = (a, b) => a >= b
const $$and = (a, b) => a && b
const $$or = (a, b) => a || b
const $$not = (a) => !a
const $$string$concat = (a, b) => a.concat(b)
const $$string$from = (a) => JSON.stringify(a)
const $$string$toList = (a) => a.split('')
const $$car = (l) => l[0]
const $$cdr = (l) => l.slice(1)
const $$cons = (x, xs) => [x, ...xs]
const $$get = (x, xs) => xs[xs.findIndex((el) => el === x) + 1]
const $$nth = (n, xs) => xs[n]
const $$equal = (a, b) => a >= b && a <= b
const $$eq = (a, b) => $$equal(a, b)
const $$list = (a) => Array.isArray(a)
const $$integer = (a) => Number.isInteger(a)
const $$float = (a) => !$$integer && typeof a === 'number'
const $$string = (a) => typeof a === 'string'
const $$character = (a) => $$string(a) && a.length === 1
const $$bool = (a) => typeof a === 'boolean'
const $$do = (...k) => k[k.length - 1]
const $$io$read = () => prompt('')
const $$io$write = (s) => {
  console.log(s)
  return null
}
const $$io$panic = (e) => {
  throw JSON.stringify(e)
}
