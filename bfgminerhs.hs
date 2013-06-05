foreign import ccall "main_body_no_arg" cMainBodyNoArg :: IO ()

main :: IO ()
main = cMainBodyNoArg
