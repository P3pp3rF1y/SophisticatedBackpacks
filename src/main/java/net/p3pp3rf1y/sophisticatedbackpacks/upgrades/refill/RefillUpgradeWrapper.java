package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.refill;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.items.CapabilityItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.api.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

import javax.annotation.Nullable;
import java.util.function.Consumer;

public class RefillUpgradeWrapper extends UpgradeWrapperBase<RefillUpgradeWrapper, RefillUpgradeItem>
		implements IFilteredUpgrade, ITickableUpgrade {
	private static final int COOLDOWN = 5;

	private final FilterLogic filterLogic;

	public RefillUpgradeWrapper(ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(upgrade, upgradeSaveHandler);
		filterLogic = new FilterLogic(upgrade, upgradeSaveHandler, Config.COMMON.refillUpgrade.filterSlots.get());
		filterLogic.setAllowByDefault();
	}

	@Override
	public FilterLogic getFilterLogic() {
		return filterLogic;
	}

	@Override
	public void tick(@Nullable PlayerEntity player, World world, BlockPos pos, IBackpackWrapper wrapper) {
		if (player == null /*not supported in block form*/ || isInCooldown(world)) {
			return;
		}
		player.getCapability(CapabilityItemHandler.ITEM_HANDLER_CAPABILITY, null).ifPresent(playerInvHandler -> InventoryHelper.iterate(filterLogic.getFilterHandler(), (slot, filter) -> {
			if (filter.isEmpty()) {
				return;
			}
			int missingCount = InventoryHelper.getCountMissingInHandler(playerInvHandler, filter, filter.getMaxStackSize());
			if (missingCount == 0) {
				return;
			}
			InventoryHelper.moveBetweenInventories(wrapper.getInventoryHandler(), playerInvHandler, filter, missingCount);
		}));
		setCooldown(world, COOLDOWN);
	}
}
