function Meta(meta)
  if meta.logo then
    local logo_path = pandoc.utils.stringify(meta.logo)
    local header = [[
\AtBeginDocument{%
    \AddToShipoutPicture*{%
         % Logo
        \AtPageLowerLeft{%
            \put(\LenToUnit{\dimexpr\paperwidth-168mm}, 23.2cm){%
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