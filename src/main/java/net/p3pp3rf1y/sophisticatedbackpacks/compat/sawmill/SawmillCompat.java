package net.p3pp3rf1y.sophisticatedbackpacks.compat.sawmill;

import net.mehvahdjukaar.sawmill.integration.emi.EMIPlugin;
import net.mehvahdjukaar.sawmill.integration.jei.JEIPlugin;
import net.mehvahdjukaar.sawmill.integration.rei.REIPlugin;
import net.minecraft.core.registries.Registries;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.fml.ModList;
import net.neoforged.fml.loading.FMLEnvironment;
import net.neoforged.neoforge.registries.DeferredHolder;
import net.neoforged.neoforge.registries.RegisterEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.emi.BackpackEmiPlugin;
import net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.jei.BackpackJeiPlugin;
import net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.rei.BackpackReiClientPlugin;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerRegistry;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedcore.compat.CompatModIds;
import net.p3pp3rf1y.sophisticatedcore.compat.ICompat;
import net.p3pp3rf1y.sophisticatedcore.compat.sawmill.SawmillUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.compat.sawmill.SawmillUpgradeItem;

import java.util.function.Supplier;

public class SawmillCompat implements ICompat {

	public static final DeferredHolder<Item, SawmillUpgradeItem> SAWMILL_UPGRADE = ModItems.ITEMS.register("sawmill/sawmill_upgrade",
			() -> new SawmillUpgradeItem(Config.SERVER.maxUpgradesPerStorage));

	@Override
	public void init(IEventBus modBus) {
		modBus.addListener(this::registerContainers);

		if (ModList.get().isLoaded(CompatModIds.JEI)) {
			((Supplier<Runnable>) () -> () -> BackpackJeiPlugin.addAdditionalCatalystRegistrar(registration -> {
				registration.addRecipeCatalyst(new ItemStack(SAWMILL_UPGRADE.get()), JEIPlugin.WOODCUTTING_RECIPE_TYPE);
			})).get().run();
		}
		if (ModList.get().isLoaded(CompatModIds.EMI)) {
			((Supplier<Runnable>) () -> () -> BackpackEmiPlugin.addAdditionalWorkstations(registration -> {
				registration.addWorkstation(EMIPlugin.WOODCUTTING_CATEGORY, SAWMILL_UPGRADE.get());
			})).get().run();
		}
		if (ModList.get().isLoaded(CompatModIds.REI) && FMLEnvironment.dist.isClient()) {
			((Supplier<Runnable>) () -> () -> BackpackReiClientPlugin.addAdditionalWorkstations(registration -> {
				registration.addWorkstations(REIPlugin.WOODCUTTING_DISPLAY, SAWMILL_UPGRADE.get());
			})).get().run();
		}
	}

	public void registerContainers(RegisterEvent event) {
		if (!event.getRegistryKey().equals(Registries.MENU)) {
			return;
		}
		UpgradeContainerType<SawmillUpgradeItem.Wrapper, SawmillUpgradeContainer> containerType = new UpgradeContainerType<>(SawmillUpgradeContainer::new);
		UpgradeContainerRegistry.register(SAWMILL_UPGRADE.getId(), containerType);
		if (FMLEnvironment.dist.isClient()) {
			SawmillCompatClient.registerUpgradeTab(containerType);
		}
	}

	@Override
	public void setup() {
		//noop
	}
}
