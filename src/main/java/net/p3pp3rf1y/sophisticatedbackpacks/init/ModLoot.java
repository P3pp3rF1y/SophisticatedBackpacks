package net.p3pp3rf1y.sophisticatedbackpacks.init;

import net.minecraft.loot.LootFunctionType;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.registry.Registry;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.data.CopyBackpackDataFunction;

public class ModLoot {
	private ModLoot() {}

	public static final LootFunctionType COPY_BACKPACK_DATA = new LootFunctionType(new CopyBackpackDataFunction.Serializer());

	public static void init() {
		Registry.register(Registry.LOOT_FUNCTION_TYPE, new ResourceLocation(SophisticatedBackpacks.MOD_ID, "copy_backpack_data"), COPY_BACKPACK_DATA);
	}
}
