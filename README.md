# Plang

The new language for programming Arduino platform.

#### Example:

```
module Main where
import Core
import Core.Native.List

class TestClass do
  arr: ptr list<int>

  TestClass do
    pass

  TestClass arr: ptr list<int> do
    this|arr = arr

Main -> int do
  arr = [int|]
  arr.add(12)
  tArr = TestClass(arr)
  arrPtr = tArr.arr
  if arrPtr? then
    for i in arr do
      println(i)
  ret 0
```
