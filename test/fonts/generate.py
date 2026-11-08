#!/usr/bin/env python3
"""Generate small synthetic font fixtures for fontsloth tests.

This is a test-time-only helper: fontTools is not a runtime dependency of
fontsloth.  It writes into the directory containing this script:

  fontsloth-test.ttf   synthetic TrueType (simple + composite glyf)
  fontsloth-test.otf   synthetic CFF1 outlines
  fontsloth-test.woff  WOFF1 wrapping fontsloth-test.ttf

The IBM Plex Serif subset is generated separately with pyftsubset; see
README.org in this directory for the command.
"""
from pathlib import Path

from fontTools.fontBuilder import FontBuilder
from fontTools.pens.t2CharStringPen import T2CharStringPen
from fontTools.pens.ttGlyphPen import TTGlyphPen
from fontTools.ttLib import TTCollection, TTFont

HERE = Path(__file__).resolve().parent

UPM = 1000
ASCENT, DESCENT = 800, -200
FAMILY = "Fontsloth Test"
STYLE = "Regular"
# Fixed timestamp (2024-01-01 in the 1904 TrueType epoch).
BUILD_TIME = 3786825600


def draw_a(pen):
    pen.moveTo((50, 0))
    pen.lineTo((250, 700))
    pen.lineTo((400, 700))
    pen.lineTo((600, 0))
    pen.closePath()
    pen.moveTo((150, 100))
    pen.lineTo((500, 100))
    pen.closePath()


def draw_b(pen):
    # Quadratic curves: exercises the TrueType quad-to path.
    pen.moveTo((100, 0))
    pen.qCurveTo((700, 0), (700, 350))
    pen.qCurveTo((700, 700), (100, 700))
    pen.closePath()


def draw_acute(pen):
    pen.moveTo((100, 750))
    pen.lineTo((200, 900))
    pen.lineTo((250, 730))
    pen.closePath()


def draw_box(pen):
    pen.moveTo((100, 100))
    pen.lineTo((700, 100))
    pen.lineTo((700, 700))
    pen.lineTo((100, 700))
    pen.closePath()


def draw_empty(pen):
    pass


def tt_glyphs(drawers):
    glyphs = {}
    glyph_set = {name: None for name in drawers} | {"Aacute": None}
    for name, draw in drawers.items():
        pen = TTGlyphPen(glyph_set)
        draw(pen)
        glyphs[name] = pen.glyph()
    # Composite: Aacute = A + acute.
    pen = TTGlyphPen(glyph_set)
    pen.addComponent("A", (1, 0, 0, 1, 0, 0))
    pen.addComponent("acute", (1, 0, 0, 1, 0, 0))
    glyphs["Aacute"] = pen.glyph()
    return glyphs


def cff_charstrings(drawers, widths):
    out = {}
    for name, draw in drawers.items():
        pen = T2CharStringPen(widths[name], None)
        draw(pen)
        out[name] = pen.getCharString()
    # CFF has no composite glyphs here, so give Aacute its own outline.
    pen = T2CharStringPen(widths["Aacute"], None)
    draw_a(pen)
    out["Aacute"] = pen.getCharString()
    return out


def name_table():
    return {
        1: FAMILY,
        2: STYLE,
        4: f"{FAMILY} {STYLE}",
        6: "FontslothTest-Regular",
        16: FAMILY,
        17: STYLE,
    }


def setup_common(fb, glyph_order, cmap, widths):
    fb.setupGlyphOrder(glyph_order)
    fb.setupCharacterMap(cmap)
    fb.setupHorizontalMetrics({n: (w, 0) for n, w in widths.items()})
    fb.setupHorizontalHeader(ascent=ASCENT, descent=DESCENT, lineGap=0)
    fb.setupNameTable(name_table())
    fb.setupOS2(
        sTypoAscender=ASCENT,
        sTypoDescender=DESCENT,
        sTypoLineGap=0,
        usWinAscent=ASCENT,
        usWinDescent=-DESCENT,
    )
    fb.setupPost()
    # Keep the generated fixtures byte-for-byte reproducible.
    fb.font["head"].created = BUILD_TIME
    fb.font["head"].modified = BUILD_TIME


def main():
    drawers = {
        ".notdef": draw_box,
        "space": draw_empty,
        "A": draw_a,
        "B": draw_b,
        "acute": draw_acute,
        "smile": draw_box,
    }
    widths = {
        ".notdef": 600,
        "space": 300,
        "A": 600,
        "B": 650,
        "acute": 300,
        "smile": 800,
        "Aacute": 600,
    }
    glyph_order = [".notdef", "space", "A", "B", "acute", "smile", "Aacute"]
    cmap = {
        0x20: "space",
        0x41: "A",
        0x42: "B",
        0xB4: "acute",
        0xC1: "Aacute",
        # Non-BMP code point forces a format 12 cmap subtable.
        ord("\U0001F600"): "smile",
    }

    ttf_path = HERE / "fontsloth-test.ttf"

    fb = FontBuilder(UPM, isTTF=True)
    setup_common(fb, glyph_order, cmap, widths)
    fb.setupGlyf(tt_glyphs(drawers))
    fb.setupDummyDSIG()
    fb.save(ttf_path)

    # A minimal variable font: one wght axis, two named instances.  It is
    # only for fvar parsing; no gvar deltas are needed.
    vf = FontBuilder(UPM, isTTF=True)
    setup_common(vf, glyph_order, cmap, widths)
    vf.setupGlyf(tt_glyphs(drawers))
    vf.setupFvar(
        [("wght", 100, 400, 900, "Weight")],
        [
            {"location": {"wght": 400}, "stylename": "Regular"},
            {"location": {"wght": 700}, "stylename": "Bold"},
        ],
    )
    vf.save(HERE / "fontsloth-test-vf.ttf")

    fb = FontBuilder(UPM, isTTF=False)
    setup_common(fb, glyph_order, cmap, widths)
    fb.setupCFF(
        "FontslothTest-Regular",
        {"FullName": f"{FAMILY} {STYLE}", "FamilyName": FAMILY, "Weight": STYLE},
        cff_charstrings(drawers, widths),
        {},
    )
    fb.setupDummyDSIG()
    fb.save(HERE / "fontsloth-test.otf")

    tt = TTFont(ttf_path)
    tt.flavor = "woff"
    tt.save(HERE / "fontsloth-test.woff")

    collection = TTCollection()
    collection.fonts = [TTFont(ttf_path), TTFont(HERE / "fontsloth-test.otf")]
    collection.save(HERE / "fontsloth-test.ttc")

    cff2_collection = TTCollection()
    cff2_collection.fonts = [
        TTFont(ttf_path),
        TTFont(HERE / "Cantarell-VF-subset.otf"),
    ]
    cff2_collection.save(HERE / "fontsloth-test-cff2.ttc")

    print(f"wrote fixtures in {HERE}")


if __name__ == "__main__":
    main()
