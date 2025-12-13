package net.p3pp3rf1y.sophisticatedbackpacks.compat.accessories;

import io.wispforest.accessories.api.AccessoriesAPI;
import io.wispforest.accessories.api.AccessoriesCapability;
import io.wispforest.accessories.api.AccessoriesContainer;
import io.wispforest.accessories.api.Accessory;
import net.minecraft.core.Holder;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.neoforged.fml.loading.FMLEnvironment;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.compat.CompatModIds;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.compat.ICompat;

import java.util.Collections;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.function.Function;

public class AccessoriesCompat implements ICompat {
	private final Set<String> backpackAccessoriesSlotNames = new CopyOnWriteArraySet<>();
	private long lastTagsRefresh = -1;
	private static final int TAGS_REFRESH_COOLDOWN = 100;
	private static final Accessory NOT_EQUIPPABLE_FROM_USE_ACCESSORY = new Accessory() {
		@Override
		public boolean canEquipFromUse(ItemStack stack) {
			return false;
		}
	};

	@Override
	public void setup() {
		addPlayerInventoryHandlers();
		ModItems.ITEMS.getEntries().stream().map(Holder::value).filter(BackpackItem.class::isInstance).forEach(item ->
				AccessoriesAPI.registerAccessory(item, NOT_EQUIPPABLE_FROM_USE_ACCESSORY));
		if (FMLEnvironment.dist.isClient()) {
			AccessoriesCompatClient.registerRenderers();
		}
	}

	private void addPlayerInventoryHandlers() {
		PlayerInventoryProvider.get().addPlayerInventoryHandler(CompatModIds.ACCESSORIES, this::getAccessoriesSlotTags,
				(player, identifier) -> getFromAccessoriesStorage(player, identifier, AccessoriesContainer::getSize, 0),
				(player, identifier, slot) -> getFromAccessoriesStorage(player, identifier, storage -> storage.getAccessories().getItem(slot), ItemStack.EMPTY),
				false, true, true, true);
	}

	private Set<String> getAccessoriesSlotTags(Player player) {
		long gameTime = player.level().getGameTime();
		if (lastTagsRefresh + TAGS_REFRESH_COOLDOWN < gameTime) {
			lastTagsRefresh = gameTime;
			backpackAccessoriesSlotNames.clear();

			backpackAccessoriesSlotNames.addAll(AccessoriesCapability.getOptionally(player)
					.map(capability -> capability.getContainers().keySet()
					).orElse(Collections.emptySet()));
		}
		return backpackAccessoriesSlotNames;
	}

	public static <T> T getFromAccessoriesStorage(LivingEntity livingEntity,
												  String identifier,
												  Function<AccessoriesContainer, T> getFromStorage,
												  T defaultValue) {
		return AccessoriesCapability.getOptionally(livingEntity)
				.map(cap -> {
					AccessoriesContainer containerObj = cap.getContainers().get(identifier);
					if (!(containerObj instanceof AccessoriesContainer storage)) {
						return defaultValue;
					}
					return getFromStorage.apply(storage);
				})
				.orElse(defaultValue);
	}

}
