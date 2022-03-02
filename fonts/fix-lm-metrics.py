# This FontForge script patches fonts in GUST’s Latin Modern family to fix two
# issues with their metrics:
#
#   1. In the OS/2 table, the USE_TYPO_METRICS bit of the fsSelection field is
#      not set.
#
#   2. The metrics in the hhea table differ (significantly) from the metrics in
#      the OS/2 table.
#
# (These issues are only present in the Latin Modern *text* fonts. Latin Modern
#  Math does not have this issue, so it does not need to be patched.)
#
# These issues are unintentional, as the following discussion confirms:
#   <https://www.man.torun.pl/archives/arc/gust-l/2021-12/msg00000.html>
#   <https://www.man.torun.pl/archives/arc/gust-l/2022-01/msg00000.html>
#
# Unfortunately, fixes have not, as of this writing, been released to CTAN, so
# this script is needed as a workaround in the meantime.

import fontforge

out_family = 'Blackboard Modern'

def fix_lm(in_path, out_family_base, out_style):
    lm = fontforge.open(in_path)

    lm.hhea_ascent = lm.os2_typoascent
    lm.hhea_descent = lm.os2_typodescent
    lm.hhea_linegap = lm.os2_typolinegap
    lm.os2_use_typo_metrics = True

    out_name = f'BlackboardModern{out_family_base}-{out_style}'
    out_family = f'Blackboard Modern {out_family_base}'

    def update_sfnt_name(language, strid, in_str):
        out_str = in_str
        if language == 'English (US)':
            if strid in ['Family', 'Preferred Family']:
                out_str = out_family
            elif strid in ['Styles (SubFamily)', 'Preferred Styles']:
                out_str = out_style
            elif strid == 'Compatible Full':
                out_str = out_name
            elif strid == 'Version':
                out_str = '2.005.1'
            elif strid == 'UniqueID':
                out_str = f'2.005.1;{out_name}'
        return (language, strid, out_str)

    lm.fontname = out_name
    lm.fullname = out_name
    lm.familyname = out_family
    lm.sfnt_names = tuple(update_sfnt_name(*tup) for tup in lm.sfnt_names)
    lm.generate(f'{out_name}.otf')

fix_lm('/usr/share/texmf/fonts/opentype/public/lm/lmroman10-regular.otf',
       'Roman', 'Regular')
fix_lm('/usr/share/texmf/fonts/opentype/public/lm/lmroman10-bold.otf',
       'Roman', 'Bold')
fix_lm('/usr/share/texmf/fonts/opentype/public/lm/lmroman10-italic.otf',
       'Roman', 'Italic')
fix_lm('/usr/share/texmf/fonts/opentype/public/lm/lmroman10-bolditalic.otf',
       'Roman', 'BoldItalic')

fix_lm('/usr/share/texmf/fonts/opentype/public/lm/lmsans10-regular.otf',
       'Sans', 'Regular')
fix_lm('/usr/share/texmf/fonts/opentype/public/lm/lmsans10-bold.otf',
       'Sans', 'Bold')
