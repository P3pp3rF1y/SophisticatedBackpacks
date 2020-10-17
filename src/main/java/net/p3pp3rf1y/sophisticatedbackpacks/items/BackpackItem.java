package net.p3pp3rf1y.sophisticatedbackpacks.items;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.inventory.container.SimpleNamedContainerProvider;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.util.ActionResult;
import net.minecraft.util.Hand;
import net.minecraft.world.World;
import net.minecraftforge.fml.network.NetworkHooks;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;

public class BackpackItem extends Item {
	private final int numberOfSlots;
	private final ScreenProperties screenProperties;

	public BackpackItem(String registryName, int numberOfSlots) {
		this(registryName, numberOfSlots, new ScreenProperties());
	}

	public BackpackItem(String registryName, int numberOfSlots, ScreenProperties screenProperties) {
		super(new Properties().maxStackSize(1).group(SophisticatedBackpacks.ITEM_GROUP));
		this.numberOfSlots = numberOfSlots;
		this.screenProperties = screenProperties;
		setRegistryName(SophisticatedBackpacks.MOD_ID, registryName);
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
}
