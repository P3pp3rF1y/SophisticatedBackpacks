package net.p3pp3rf1y.sophisticatedbackpacks.init;

import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.neoforged.bus.api.EventPriority;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.neoforge.capabilities.Capabilities;
import net.neoforged.neoforge.capabilities.RegisterCapabilitiesEvent;
import net.neoforged.neoforge.common.NeoForge;
import net.neoforged.neoforge.registries.DeferredRegister;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlock;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlockEntity;

import java.util.function.Supplier;

public class ModBlocks {
	private static final DeferredRegister<Block> BLOCKS = DeferredRegister.create(BuiltInRegistries.BLOCK, SophisticatedBackpacks.MOD_ID);
	private static final DeferredRegister<BlockEntityType<?>> BLOCK_ENTITY_TYPES = DeferredRegister.create(BuiltInRegistries.BLOCK_ENTITY_TYPE,
			SophisticatedBackpacks.MOD_ID);

	private ModBlocks() {
	}

	public static final Supplier<BackpackBlock> BACKPACK = BLOCKS.register("backpack", () -> new BackpackBlock());
	public static final Supplier<BackpackBlock> COPPER_BACKPACK = BLOCKS.register("copper_backpack", () -> new BackpackBlock());
	public static final Supplier<BackpackBlock> IRON_BACKPACK = BLOCKS.register("iron_backpack", () -> new BackpackBlock());
	public static final Supplier<BackpackBlock> GOLD_BACKPACK = BLOCKS.register("gold_backpack", () -> new BackpackBlock());
	public static final Supplier<BackpackBlock> DIAMOND_BACKPACK = BLOCKS.register("diamond_backpack", () -> new BackpackBlock());
	public static final Supplier<BackpackBlock> NETHERITE_BACKPACK = BLOCKS.register("netherite_backpack", () -> new BackpackBlock(1200));

	@SuppressWarnings("ConstantConditions") // no datafixer type needed
	public static final Supplier<BlockEntityType<BackpackBlockEntity>> BACKPACK_TILE_TYPE = BLOCK_ENTITY_TYPES.register("backpack",
			() -> BlockEntityType.Builder.of(BackpackBlockEntity::new, BACKPACK.get(), COPPER_BACKPACK.get(), IRON_BACKPACK.get(), GOLD_BACKPACK.get(),
					DIAMOND_BACKPACK.get(), NETHERITE_BACKPACK.get()).build(null));

	public static void registerHandlers(IEventBus modBus) {
		BLOCKS.register(modBus);
		BLOCK_ENTITY_TYPES.register(modBus);
		NeoForge.EVENT_BUS.addListener(EventPriority.LOWEST, BackpackBlock::playerInteract);
		modBus.addListener(ModBlocks::registerCapabilities);
	}

	private static void registerCapabilities(RegisterCapabilitiesEvent event) {
		event.registerBlockEntity(Capabilities.ItemHandler.BLOCK, BACKPACK_TILE_TYPE.get(), BackpackBlockEntity::getExternalItemHandler);
		event.registerBlockEntity(Capabilities.FluidHandler.BLOCK, BACKPACK_TILE_TYPE.get(), BackpackBlockEntity::getExternalFluidHandler);
		event.registerBlockEntity(Capabilities.EnergyStorage.BLOCK, BACKPACK_TILE_TYPE.get(), BackpackBlockEntity::getExternalEnergyStorage);
	}
}
