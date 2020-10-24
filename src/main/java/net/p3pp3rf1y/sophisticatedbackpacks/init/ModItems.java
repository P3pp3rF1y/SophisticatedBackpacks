package net.p3pp3rf1y.sophisticatedbackpacks.init;

import net.minecraft.client.gui.ScreenManager;
import net.minecraft.inventory.container.ContainerType;
import net.minecraft.item.Item;
import net.minecraft.item.crafting.IRecipeSerializer;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.common.extensions.IForgeContainerType;
import net.minecraftforge.event.RegistryEvent;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.registries.IForgeRegistry;
import net.minecraftforge.registries.ObjectHolder;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackUpgradeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.items.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.items.ItemBase;
import net.p3pp3rf1y.sophisticatedbackpacks.items.PickupUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.items.ScreenProperties;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InjectionHelper;

@ObjectHolder(SophisticatedBackpacks.MOD_ID)
public class ModItems {
	private ModItems() {}

	public static final BackpackItem BACKPACK = InjectionHelper.nullValue();
	public static final BackpackItem IRON_BACKPACK = InjectionHelper.nullValue();
	public static final BackpackItem GOLD_BACKPACK = InjectionHelper.nullValue();
	public static final BackpackItem DIAMOND_BACKPACK = InjectionHelper.nullValue();

	public static void registerHandlers(IEventBus modBus) {
		modBus.addGenericListener(Item.class, ModItems::register);
		modBus.addGenericListener(ContainerType.class, ModItems::registerContainers);
		modBus.addGenericListener(IRecipeSerializer.class, ModItems::registerRecipeSerializers);
	}

	public static void register(RegistryEvent.Register<Item> event) {
		IForgeRegistry<Item> reg = event.getRegistry();
		reg.register(new BackpackItem("backpack", 27, 1));
		reg.register(new BackpackItem("iron_backpack", 54, 2));
		reg.register(new BackpackItem("gold_backpack", 81, 3, new ScreenProperties().setTextureSize(512)));
		reg.register(new BackpackItem("diamond_backpack", 108, 5, new ScreenProperties().setSlotsOnLine(12).setPlayerInventoryYOffset(27).setTextureSize(512)));
		reg.register(new ItemBase("upgrade_base", new Item.Properties().maxStackSize(16)));
		reg.register(new PickupUpgrade());
	}

	public static void registerContainers(RegistryEvent.Register<ContainerType<?>> evt) {
		IForgeRegistry<ContainerType<?>> r = evt.getRegistry();
		ContainerType<BackpackContainer> backpackContainerType = IForgeContainerType.create(BackpackContainer::fromBuffer);
		//noinspection ConstantConditions
		r.register(backpackContainerType.setRegistryName(BACKPACK.getRegistryName()));

		DistExecutor.runWhenOn(Dist.CLIENT, () -> () -> ScreenManager.registerFactory(backpackContainerType, BackpackScreen::new));
	}

	public static void registerRecipeSerializers(RegistryEvent.Register<IRecipeSerializer<?>> evt) {
		evt.getRegistry().register(BackpackUpgradeRecipe.SERIALIZER.setRegistryName(SophisticatedBackpacks.MOD_ID, "backpack_upgrade"));
	}
}
