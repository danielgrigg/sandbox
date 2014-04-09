XBUILD=/Applications/Xcode.app/Contents/Developer/usr/bin/xcodebuild
PRODUCT=bullet2
PRODUCT_ROOT=../../downloads/${PRODUCT}
PRODUCT_BUILD=$(PRODUCT_ROOT)/build/xcode4ios
PROJECT_LINEAR_MATH=$(PRODUCT_BUILD)/LinearMath.xcodeproj
PROJECT_BULLET_COLLISION=$(PRODUCT_BUILD)/BulletCollision.xcodeproj
PROJECT_BULLET_DYNAMICS=$(PRODUCT_BUILD)/BulletDynamics.xcodeproj
PRODUCT_OUT_DIR=${PRODUCT_ROOT}/lib

OPTS=-verbose
RELEASE_i386=VALID_ARCHS="i386 x86_64" -sdk iphonesimulator -arch i386 -configuration "Release Native 32-bit"
RELEASE_x64=VALID_ARCHS="i386 x86_64" -sdk iphonesimulator -arch x86_64 -configuration "Release Native 64-bit"
RELEASE_ARMV7=VALID_ARCHS="armv7 armv7s arm64" -sdk iphoneos -arch armv7 -configuration "Release Native 32-bit"
RELEASE_ARMV7S=VALID_ARCHS="armv7 armv7s arm64" -sdk iphoneos -arch armv7s -configuration "Release Native 32-bit"
RELEASE_ARM64=VALID_ARCHS="armv7 armv7s arm64" -sdk iphoneos -arch arm64 -configuration "Release Native 64-bit"

all: libs

bootstrap: 
	${PRODUCT_ROOT}/build/premake4_osx --file=${PRODUCT_ROOT}/build/premake4.lua --ios xcode4

install: all
	@mkdir -p ../include/bullet ../lib
	@cp $(PRODUCT_ROOT)/src/*.h ../include/bullet/
	@cp libLinearMathUniversal.a libBulletCollisionUniversal.a libBulletDynamicsUniversal.a ../lib/
	@rsync -a --include '*/' --include '*.h' --exclude='*' $(PRODUCT_ROOT)/src/ ../include/bullet/

libs: libLinearMathUniversal.a libBulletCollisionUniversal.a libBulletDynamicsUniversal.a

libLinearMathUniversal.a: libLinearMath_i386.a libLinearMath_x86_64.a libLinearMath_armv7.a libLinearMath_armv7s.a libLinearMath_arm64.a
	@lipo -create -output $@ $^

libLinearMath_i386.a:
	$(XBUILD) ${OPTS} -project $(PROJECT_LINEAR_MATH) ${RELEASE_i386} build
	@mv $(PRODUCT_OUT_DIR)/libLinearMath_xcode4.a $@

libLinearMath_x86_64.a:
	$(XBUILD) ${OPTS} -project $(PROJECT_LINEAR_MATH) ${RELEASE_x64} build
	@mv $(PRODUCT_OUT_DIR)/libLinearMath_xcode4_x64_release.a $@

libLinearMath_armv7.a:
	$(XBUILD) -project $(PROJECT_LINEAR_MATH) ${OPTS} ${RELEASE_ARMV7} build
	@mv $(PRODUCT_OUT_DIR)/libLinearMath_xcode4.a $@

libLinearMath_armv7s.a:
	$(XBUILD) ${OPTS} -project $(PROJECT_LINEAR_MATH) ${RELEASE_ARMV7S} build
	@mv $(PRODUCT_OUT_DIR)/libLinearMath_xcode4.a $@

libLinearMath_arm64.a:
	$(XBUILD) ${OPTS} -project $(PROJECT_LINEAR_MATH) ${RELEASE_ARM64} build
	@mv $(PRODUCT_OUT_DIR)/libLinearMath_xcode4_x64_release.a $@


libBulletCollisionUniversal.a: libBulletCollision_i386.a libBulletCollision_x86_64.a libBulletCollision_armv7.a libBulletCollision_armv7s.a libBulletCollision_arm64.a
	@lipo -create -output $@ $^

libBulletCollision_i386.a:
	$(XBUILD) ${OPTS} -project $(PROJECT_BULLET_COLLISION) ${RELEASE_i386} build
	@mv $(PRODUCT_OUT_DIR)/libBulletCollision_xcode4.a $@

libBulletCollision_x86_64.a:
	$(XBUILD) ${OPTS} -project $(PROJECT_BULLET_COLLISION) ${RELEASE_x64} build
	@mv $(PRODUCT_OUT_DIR)/libBulletCollision_xcode4_x64_release.a $@

libBulletCollision_armv7.a:
	$(XBUILD) -project $(PROJECT_BULLET_COLLISION) ${OPTS} ${RELEASE_ARMV7} build
	@mv $(PRODUCT_OUT_DIR)/libBulletCollision_xcode4.a $@

libBulletCollision_armv7s.a:
	$(XBUILD) ${OPTS} -project $(PROJECT_BULLET_COLLISION) ${RELEASE_ARMV7S} build
	@mv $(PRODUCT_OUT_DIR)/libBulletCollision_xcode4.a $@

libBulletCollision_arm64.a:
	$(XBUILD) ${OPTS} -project $(PROJECT_BULLET_COLLISION) ${RELEASE_ARM64} build
	@mv $(PRODUCT_OUT_DIR)/libBulletCollision_xcode4_x64_release.a $@


libBulletDynamicsUniversal.a: libBulletDynamics_i386.a libBulletDynamics_x86_64.a libBulletDynamics_armv7.a libBulletDynamics_armv7s.a libBulletDynamics_arm64.a
	@lipo -create -output $@ $^

libBulletDynamics_i386.a:
	$(XBUILD) ${OPTS} -project $(PROJECT_BULLET_DYNAMICS) ${RELEASE_i386} build
	@mv $(PRODUCT_OUT_DIR)/libBulletDynamics_xcode4.a $@

libBulletDynamics_x86_64.a:
	$(XBUILD) ${OPTS} -project $(PROJECT_BULLET_DYNAMICS) ${RELEASE_x64} build
	@mv $(PRODUCT_OUT_DIR)/libBulletDynamics_xcode4_x64_release.a $@

libBulletDynamics_armv7.a:
	$(XBUILD) -project $(PROJECT_BULLET_DYNAMICS) ${OPTS} ${RELEASE_ARMV7} build
	@mv $(PRODUCT_OUT_DIR)/libBulletDynamics_xcode4.a $@

libBulletDynamics_armv7s.a:
	$(XBUILD) ${OPTS} -project $(PROJECT_BULLET_DYNAMICS) ${RELEASE_ARMV7S} build
	@mv $(PRODUCT_OUT_DIR)/libBulletDynamics_xcode4.a $@

libBulletDynamics_arm64.a:
	$(XBUILD) ${OPTS} -project $(PROJECT_BULLET_DYNAMICS) ${RELEASE_ARM64} build
	@mv $(PRODUCT_OUT_DIR)/libBulletDynamics_xcode4_x64_release.a $@


clean:
	@rm -f *.a
	@rm -rf ../include/bullet
	@rm -rf $(PRODUCT_BUILD)
	@rm -f ../lib/{libBullet*,libLinearMath*}
