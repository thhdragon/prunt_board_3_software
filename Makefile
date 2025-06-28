.PHONY: kalico/out/klipper.bin firmware/bin/prunt_board_3_firmware.elf firmware/firmware_patcher/bin/firmware_patcher server/bin/prunt_board_3_server server/src/embedded_resources.adb
# The above all use their own build systems.

server/bin/prunt_board_3_server: server/src/embedded_resources.adb
	cd server && alr build

server/embedded_resources/prunt_board_3_firmware_with_crc.bin: firmware/bin/prunt_board_3_firmware_with_crc.bin
	cp firmware/bin/prunt_board_3_firmware_with_crc.bin server/embedded_resources/prunt_board_3_firmware_with_crc.bin

server/src/embedded_resources.adb: server/embedded_resources/prunt_board_3_firmware_with_crc.bin
	make -C server/embedded_resources

kalico/out/klipper.bin:
	make -C kalico

firmware/firmware_patcher/bin/firmware_patcher:
	cd firmware/firmware_patcher && alr build

firmware/bin/prunt_board_3_firmware.elf:
	cd firmware && ADL_BUILD=Production alr build

firmware/bin/prunt_board_3_firmware.bin: firmware/bin/prunt_board_3_firmware.elf
	cd firmware && alr exec -- arm-eabi-objcopy -O binary bin/prunt_board_3_firmware.elf bin/prunt_board_3_firmware.bin

firmware/bin/prunt_board_3_firmware_with_crc.bin: firmware/bin/prunt_board_3_firmware.bin kalico/out/klipper.bin firmware/firmware_patcher/bin/firmware_patcher
	cd firmware && ./firmware_patcher/bin/firmware_patcher bin/prunt_board_3_firmware.bin ../kalico/out/klipper.bin bin/prunt_board_3_firmware_with_crc.bin
