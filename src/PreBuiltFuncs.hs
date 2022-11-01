module PreBuiltFuncs where 

import Object 

data PBF = LEN | APPEND

getLength :: Object -> Int 
getLength o = i 
  where 
    i 
      | objectType o == ARRAY_OBJ = length (arrValue o) 
      | objectType o == MAP_OBJ = length (mapValue o) 
      | objectType o == STRING_OBJ = length (stringValue o)
      | otherwise = error ("can't get length of type: " ++ (show (objectType o)))

appendArr :: (Object, Object) -> Object 
appendArr (o1, o2) = ob 
  where 
    ob 
      | objectType  o1 =/ ARRAY_OBJ = error ("can't append to type: " ++ show(objectType o1))
      | objectType o2 == ARRAY_OBJ || objectType o2 == MAP_OBJ = error "havn't implemented nested map/array"
      | otherwise = ArrayObject{objectType = ARRAY_OBJ, arrValue = arrValue o1 ++ [o2]}
