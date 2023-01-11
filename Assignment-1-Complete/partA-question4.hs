nulll ns = if length ns > 0 then False else True

safetailGuard ns    | nulll ns == True = []
                    | nulll ns == False = tail ns

safetailConditional ns = if nulll ns then [] else tail ns

safetailPattern [] = []
safetailPattern ns = tail ns