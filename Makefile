NUNIT ?= nunit-console

SRCS  := $(wildcard SharpXml/*.fs SharpXml.Common/*.fs)
TSRCS := $(wildcard SharpXml.Tests/*.fs)

DEBUG ?= 0
ifeq ($(DEBUG), 1)
    BUILD=Debug
else
    BUILD=Release
endif

all: SharpXml/bin/$(BUILD)/SharpXml.dll

rebuild: clean all

SharpXml/bin/$(BUILD)/SharpXml.dll: $(SRCS)
	@cd SharpXml && xbuild /p:Configuration=$(BUILD)

SharpXml.Tests/bin/$(BUILD)/SharpXml.Tests.dll: $(SRCS) $(TSRCS)
	@cd SharpXml.Tests && xbuild /p:Configuration=$(BUILD)

check: SharpXml.Tests/bin/$(BUILD)/SharpXml.Tests.dll
	@$(NUNIT) -framework=4.0 -exclude=Profiling -noxml -nologo $<

clean:
	@xbuild /t:Clean /p:Configuration=$(BUILD)
