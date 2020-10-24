package net.p3pp3rf1y.sophisticatedbackpacks.items;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.inventory.container.SimpleNamedContainerProvider;
import net.minecraft.item.DyeColor;
import net.minecraft.item.ItemGroup;
import net.minecraft.item.ItemStack;
import net.minecraft.util.ActionResult;
import net.minecraft.util.Hand;
import net.minecraft.util.NonNullList;
import net.minecraft.world.World;
import net.minecraftforge.fml.network.NetworkHooks;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackWrapper;

public class BackpackItem extends ItemBase {
	private final int numberOfSlots;
	private final int numberOfUpgradeSlots;
	private final ScreenProperties screenProperties;

	public BackpackItem(String registryName, int numberOfSlots, int numberOfUpgradeSlots) {
		this(registryName, numberOfSlots, numberOfUpgradeSlots, new ScreenProperties());
	}

	public BackpackItem(String registryName, int numberOfSlots, int numberOfUpgradeSlots, ScreenProperties screenProperties) {
		super(registryName, new Properties().maxStackSize(1));
		this.numberOfSlots = numberOfSlots;
		this.numberOfUpgradeSlots = numberOfUpgradeSlots;
		this.screenProperties = screenProperties;
	}

	@Override
	public void fillItemGroup(ItemGroup group, NonNullList<ItemStack> items) {
		super.fillItemGroup(group, items);

		if (this != ModItems.BACKPACK) {
			return;
		}

		for (DyeColor color : DyeColor.values()) {
			ItemStack stack = new ItemStack(this);
			new BackpackWrapper(stack).setColors(color.getColorValue(), color.getColorValue());
			items.add(stack);
		}

		ItemStack stack = new ItemStack(this);
		new BackpackWrapper(stack).setColors(DyeColor.YELLOW.getColorValue(), DyeColor.BLUE.getColorValue());
		items.add(stack);
	}

	@Override
	public ActionResult<ItemStack> onItemRightClick(World world, PlayerEntity player, Hand hand) {
		ItemStack stack = player.getHeldItem(hand);

		if (!world.isRemote && player instanceof ServerPlayerEntity) {
			NetworkHooks.openGui((ServerPlayerEntity) player, new SimpleNamedContainerProvider((w, p, pl) -> new BackpackContainer(w, p, stack), stack.getDisplayName()),
					buf -> buf.writeBoolean(hand == Hand.MAIN_HAND));
		}
		return ActionResult.resultSuccess(stack);
	}

	public ScreenProperties getScreenProperties() {
		return screenProperties;
	}

	public int getNumberOfSlots() {
		return numberOfSlots;
	}

	public int getNumberOfUpgradeSlots() {
		return numberOfUpgradeSlots;
	}
}