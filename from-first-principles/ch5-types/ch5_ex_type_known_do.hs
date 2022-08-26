module Ch5_Munge where

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge fx_y fy_wz x = w
    where (w, _) = fy_wz $ fx_y x

munge' :: (x -> y) -> (y -> (w, z)) -> x -> w
munge' fx_y fy_wz = fst . fy_wz . fx_y
