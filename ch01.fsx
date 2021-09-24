type seasoning =
    | Salt
    | Pepper

type num =
    | Zero
    | One_more_than of num

One_more_than Zero
One_more_than(One_more_than Zero)
One_more_than(One_more_than(One_more_than Zero))
One_more_than(One_more_than(One_more_than(One_more_than Zero)))

// One_more_than 0
// This expression was expected to have type
//     'num'
// but here has type
//     'int'

type 'a open_face_sandwich =
    | Bread of 'a
    | Slice of 'a open_face_sandwich

// alternate
// type open_face_sandwich<'a> =
//     | Bread of 'a
//     | Slice of 'a open_face_sandwich

Bread 0

Bread true
Bread(Bread 0)

Bread(One_more_than Zero)
Bread(Bread(One_more_than Zero))
