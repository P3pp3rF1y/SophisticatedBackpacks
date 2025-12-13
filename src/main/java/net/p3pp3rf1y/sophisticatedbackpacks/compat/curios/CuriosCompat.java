package net.p3pp3rf1y.sophisticatedbackpacks.compat.curios;

import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.neoforged.api.distmarker.Dist;
import net.neoforged.fml.loading.FMLEnvironment;
import net.neoforged.fml.loading.FMLLoader;
import net.p3pp3rf1y.sophisticatedbackpacks.compat.CompatModIds;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.compat.ICompat;
import top.theillusivec4.curios.api.CuriosApi;
import top.theillusivec4.curios.api.type.inventory.ICurioStacksHandler;

import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.function.Function;

public class CuriosCompat implements ICompat {

	private void addPlayerInventoryHandlers() {
		PlayerInventoryProvider.get().addPlayerInventoryHandler(CompatModIds.CURIOS, this::getCurioTags,
				(player, identifier) -> getFromCuriosSlotStackHandler(player, identifier, ICurioStacksHandler::getSlots, 0),
				(player, identifier, slot) -> getFromCuriosSlotStackHandler(player, identifier, sh -> sh.getStacks().getStackInSlot(slot), ItemStack.EMPTY),
				false, true, true, true);
	}

	private final Set<String> backpackCurioIdentifiers = new CopyOnWriteArraySet<>();
	private long lastTagsRefresh = -1;
	private static final int TAGS_REFRESH_COOLDOWN = 100;

	private Set<String> getCurioTags(Player player) {
		long gameTime = player.level().getGameTime();
		if (lastTagsRefresh + TAGS_REFRESH_COOLDOWN < gameTime) {
			lastTagsRefresh = gameTime;
			backpackCurioIdentifiers.clear();
			backpackCurioIdentifiers.addAll(CuriosApi.getItemStackSlots(ModItems.BACKPACK.get().getDefaultInstance(), FMLLoader.getDist() == Dist.CLIENT).keySet());
			backpackCurioIdentifiers.add("curio");
		}
		return backpackCurioIdentifiers;
	}

	public static <T> T getFromCuriosSlotStackHandler(LivingEntity livingEntity, String identifier, Function<ICurioStacksHandler, T> getFromHandler, T defaultValue) {
		return CuriosApi.getCuriosInventory(livingEntity)
				.map(h -> h.getStacksHandler(identifier).map(getFromHandler).orElse(defaultValue)).orElse(defaultValue);
	}

	@Override
	public void setup() {
		if (FMLEnvironment.dist.isClient()) {
			CuriosCompatClient.registerRenderers();
		}

		addPlayerInventoryHandlers();
	}
}
