.include "../../mk/bsd.prefs.mk"

GODI_PLIST=     yes
.include "../../mk/godi.pkg.mk"

VERSION=        1.1.95test1
PKGNAME=        godi-pxp-${VERSION}
DISTNAME=       trunk
DISTFILES=
CATEGORIES=     godi
MASTER_SITES=   http://www.ocaml-programming.de/packages/
MAINTAINER=     gerd@gerd-stolpmann.de
HOMEPAGE=       http://www.ocaml-programing.de/
COMMENT=        PXP is an advanced XML parser

DEPENDS+=       godi-ocaml>=3.07:../../godi/godi-ocaml
DEPENDS+=       godi-ocamlnet>0.97.1:../../godi/godi-ocamlnet
DEPENDS+=       godi-wlex>=20021107:../../godi/godi-wlex
DEPENDS+=	godi-ulex>=0:../../godi/godi-ulex
BUILD_DEPENDS+= godi-findlib>=1.0:../../godi/godi-findlib

PATH:=          ${LOCALBASE}/bin:${PATH}

HAS_CONFIGURE=  yes

# ocamlfind must install into the pkg-lib directory, not into site-lib.
# Use the build time configuration file:
MAKE_ENV+=      ${BUILD_OCAMLFIND_ENV}

USE_GMAKE=      yes

ALL_TARGET=     all
.if ${GODI_HAVE_OCAMLOPT} == "yes"
ALL_TARGET+=    opt
.endif

post-extract:
	cd ${WRKDIR} && svn co --non-interactive \
		https://gps.dynxs.de/svn/lib-pxp/trunk

post-configure:
	rm -f ${WRKSRC}/gensrc/pxp-wlex-utf8/gen_done


post-install:
	mkdir -p ${LOCALBASE}/doc/godi-pxp
.	for F in LICENSE
	    cp ${WRKSRC}/${F} ${LOCALBASE}/doc/godi-pxp
.	endfor
	#cp -R ${WRKSRC}/doc/manual/html ${LOCALBASE}/doc/godi-pxp/html

.include "../../mk/bsd.pkg.mk"
