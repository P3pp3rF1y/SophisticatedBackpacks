package net.p3pp3rf1y.sophisticatedbackpacks.init;

import net.minecraft.block.Block;
import net.minecraftforge.event.RegistryEvent;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.registries.IForgeRegistry;
import net.minecraftforge.registries.ObjectHolder;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.blocks.BackpackBlock;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InjectionHelper;

@ObjectHolder(SophisticatedBackpacks.MOD_ID)
public class ModBlocks {
	private ModBlocks() {}

	public static final BackpackBlock BACKPACK = InjectionHelper.nullValue();
	public static final BackpackBlock IRON_BACKPACK = InjectionHelper.nullValue();
	public static final BackpackBlock GOLD_BACKPACK = InjectionHelper.nullValue();
	public static final BackpackBlock DIAMOND_BACKPACK = InjectionHelper.nullValue();

	public static void registerHandlers(IEventBus modBus) {
		modBus.addGenericListener(Block.class, ModBlocks::register);
	}

	public static void register(RegistryEvent.Register<Block> event) {
		IForgeRegistry<Block> reg = event.getRegistry();
		reg.register(new BackpackBlock("backpack"));
		reg.register(new BackpackBlock("iron_backpack"));
		reg.register(new BackpackBlock("gold_backpack"));
		reg.register(new BackpackBlock("diamond_backpack"));
	}
}
