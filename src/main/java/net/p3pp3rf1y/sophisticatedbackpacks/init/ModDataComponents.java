package net.p3pp3rf1y.sophisticatedbackpacks.init;

import com.mojang.serialization.Codec;
import net.minecraft.core.component.DataComponentType;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.resources.ResourceLocation;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.neoforge.registries.DeferredRegister;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception.InventoryOrder;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.refill.RefillUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper.ToolSwapMode;

import java.util.Map;
import java.util.function.Supplier;

public class ModDataComponents {
    private static final DeferredRegister<DataComponentType<?>> DATA_COMPONENT_TYPES = DeferredRegister.create(BuiltInRegistries.DATA_COMPONENT_TYPE, SophisticatedBackpacks.MOD_ID);

    public static final Supplier<DataComponentType<ResourceLocation>> LOOT_TABLE = DATA_COMPONENT_TYPES.register("loot_table",
            () -> new DataComponentType.Builder<ResourceLocation>().persistent(ResourceLocation.CODEC).networkSynchronized(ResourceLocation.STREAM_CODEC).build());

    public static final Supplier<DataComponentType<Float>> LOOT_FACTOR = DATA_COMPONENT_TYPES.register("loot_factor",
            () -> new DataComponentType.Builder<Float>().persistent(Codec.FLOAT).networkSynchronized(ByteBufCodecs.FLOAT).build());

    public static final Supplier<DataComponentType<Integer>> COLUMNS_TAKEN = DATA_COMPONENT_TYPES.register("columns_taken",
            () -> new DataComponentType.Builder<Integer>().persistent(Codec.INT).networkSynchronized(ByteBufCodecs.INT).build());

    public static final Supplier<DataComponentType<String>> ITEM_NAME = DATA_COMPONENT_TYPES.register("item_name",
            () -> new DataComponentType.Builder<String>().persistent(Codec.STRING).networkSynchronized(ByteBufCodecs.STRING_UTF8).build());

    public static final Supplier<DataComponentType<Boolean>> FILTER_BY_INVENTORY = DATA_COMPONENT_TYPES.register("filter_by_inventory",
            () -> new DataComponentType.Builder<Boolean>().persistent(Codec.BOOL).networkSynchronized(ByteBufCodecs.BOOL).build());

    public static final Supplier<DataComponentType<InventoryOrder>> INVENTORY_ORDER = DATA_COMPONENT_TYPES.register("inventory_order",
            () -> new DataComponentType.Builder<InventoryOrder>().persistent(InventoryOrder.CODEC).networkSynchronized(InventoryOrder.STREAM_CODEC).build());

    public static final Supplier<DataComponentType<Map<Integer, RefillUpgradeWrapper.TargetSlot>>> TARGET_SLOTS = DATA_COMPONENT_TYPES.register("target_slots",
            () -> new DataComponentType.Builder<Map<Integer, RefillUpgradeWrapper.TargetSlot>>().persistent(RefillUpgradeWrapper.TARGET_SLOTS_CODEC).networkSynchronized(RefillUpgradeWrapper.TARGET_SLOTS_STREAM_CODEC).build());

    public static final Supplier<DataComponentType<Boolean>> SHOULD_SWAP_WEAPON = DATA_COMPONENT_TYPES.register("should_swap_weapon",
            () -> new DataComponentType.Builder<Boolean>().persistent(Codec.BOOL).networkSynchronized(ByteBufCodecs.BOOL).build());

    public static final Supplier<DataComponentType<ToolSwapMode>> TOOL_SWAP_MODE = DATA_COMPONENT_TYPES.register("tool_swap_mode",
            () -> new DataComponentType.Builder<ToolSwapMode>().persistent(ToolSwapMode.CODEC).networkSynchronized(ToolSwapMode.STREAM_CODEC).build());

    public static final Supplier<DataComponentType<ResourceLocation>> TEMPLATE_NAME = DATA_COMPONENT_TYPES.register("template_location",
            () -> new DataComponentType.Builder<ResourceLocation>().persistent(ResourceLocation.CODEC).networkSynchronized(ResourceLocation.STREAM_CODEC).build());

    public static void register(IEventBus modBus) {
        DATA_COMPONENT_TYPES.register(modBus);
    }
}
