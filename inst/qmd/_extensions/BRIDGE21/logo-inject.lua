function Meta(meta)
  if meta.logo then
    local logo_path = pandoc.utils.stringify(meta.logo)
    local logo_x = pandoc.utils.stringify(meta.logo_x)
    local logo_y = pandoc.utils.stringify(meta.logo_y)
    local logo_width = pandoc.utils.stringify(meta.width)
    local header = [[
\AtBeginDocument{%
    \AddToShipoutPicture*{%
         % Logo
        \AtPageLowerLeft{%
            \put(\LenToUnit{\dimexpr\paperwidth-]] .. logo_x .. [[}, ]] .. logo_y ..[[){%
            \includegraphics[width=]] .. logo_width .. [[]{]] .. logo_path .. [[}
            }%
        }%
    }
}
]]
    
    if not meta['header-includes'] then
      meta['header-includes'] = pandoc.MetaList{}
    end
    table.insert(meta['header-includes'], pandoc.RawBlock('latex', header))
  end
  return meta
end