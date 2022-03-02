# This script patches an OpenType Math font to eliminate the requirement that
# the OpenType `math` script be used to activate math shaping features.
# Unfortunately, enabling the math script is currently difficult or impossible
# in many systems, most notably when using HTML/CSS, so this workaround will
# remain necessary unless that changes.

import fontforge

def alter_math_font(in_path, out_name, out_family):
    lm = fontforge.open(in_path)

    for lookup in lm.gsub_lookups:
        _, _, features = lm.getLookupInfo(lookup)
        new_features = tuple((feature, (('DFLT', ('dflt',)),)) for feature, _ in features)
        lm.lookupSetFeatureList(lookup, new_features)

    def update_sfnt_name(language, strid, string):
        if language == 'English (US)' and strid == 'Preferred Family':
            return (language, strid, out_family)
        else:
            return (language, strid, string)

    lm.fontname = out_name
    lm.fullname = out_name
    lm.familyname = out_family
    lm.sfnt_names = tuple(update_sfnt_name(*tup) for tup in lm.sfnt_names)
    lm.generate(f'{out_name}.otf')

alter_math_font('/usr/share/texmf/fonts/opentype/public/lm-math/latinmodern-math.otf',
                'BlackboardModernMath-Regular',
                'Blackboard Modern Math')
alter_math_font('/usr/share/texmf/fonts/opentype/public/tex-gyre-math/texgyrepagella-math.otf',
                'BlackboardPagellaMath-Regular',
                'Blackboard Pagella Math')
alter_math_font('/usr/share/texmf/fonts/opentype/public/tex-gyre-math/texgyretermes-math.otf',
                'BlackboardTermesMath-Regular',
                'Blackboard Termes Math')
alter_math_font('/usr/share/texmf/fonts/opentype/public/tex-gyre-math/texgyrebonum-math.otf',
                'BlackboardBonumMath-Regular',
                'Blackboard Bonum Math')
alter_math_font('/home/alexis/Downloads/euler.otf',
                'BlackboardNeoEuler-Regular',
                'Blackboard Neo Euler')
alter_math_font('/home/alexis/Downloads/gfsneohellenicmath/GFSNeohellenicMath.otf',
                'BlackboardNeohellenicMath-Regular',
                'Blackboard Neohellenic Math')
alter_math_font('/home/alexis/Downloads/CambriaMath.otf',
                'BlackboardCambriaMath-Regular',
                'Blackboard Cambria Math')
