function Meta(meta)
  if meta.logo then
    local logo_path = pandoc.utils.stringify(meta.logo)
    local logo_position = pandoc.utils.stringify(meta.position)
    local header = [[
\AtBeginDocument{%
    \AddToShipoutPicture*{%
         % Logo
        \AtPageLowerLeft{%
            \put(\LenToUnit{\dimexpr\paperwidth-168mm}, ]] .. logo_position..[[cm){%
            \includegraphics[width=5in]{]] .. logo_path .. [[}
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