package net.p3pp3rf1y.sophisticatedbackpacks.items;

import net.minecraft.block.BlockState;
import net.minecraft.block.SoundType;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.inventory.EquipmentSlotType;
import net.minecraft.inventory.container.SimpleNamedContainerProvider;
import net.minecraft.item.BlockItemUseContext;
import net.minecraft.item.DyeColor;
import net.minecraft.item.ItemGroup;
import net.minecraft.item.ItemStack;
import net.minecraft.item.ItemUseContext;
import net.minecraft.util.ActionResult;
import net.minecraft.util.ActionResultType;
import net.minecraft.util.Hand;
import net.minecraft.util.NonNullList;
import net.minecraft.util.SoundCategory;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.fml.network.NetworkHooks;
import net.p3pp3rf1y.sophisticatedbackpacks.blocks.BackpackBlock;
import net.p3pp3rf1y.sophisticatedbackpacks.blocks.tile.BackpackTileEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedbackpacks.util.WorldHelper;

import javax.annotation.Nullable;
import java.util.function.Supplier;

public class BackpackItem extends ItemBase {
	private final int numberOfSlots;
	private final int numberOfUpgradeSlots;
	private final ScreenProperties screenProperties;
	private final Supplier<BackpackBlock> blockSupplier;

	public BackpackItem(String registryName, int numberOfSlots, int numberOfUpgradeSlots, Supplier<BackpackBlock> blockSupplier) {
		this(registryName, numberOfSlots, numberOfUpgradeSlots, new ScreenProperties(), blockSupplier);
	}

	public BackpackItem(String registryName, int numberOfSlots, int numberOfUpgradeSlots, ScreenProperties screenProperties, Supplier<BackpackBlock> blockSupplier) {
		super(registryName, new Properties().maxStackSize(1));
		this.numberOfSlots = numberOfSlots;
		this.numberOfUpgradeSlots = numberOfUpgradeSlots;
		this.screenProperties = screenProperties;
		this.blockSupplier = blockSupplier;
	}

	@Override
	public void fillItemGroup(ItemGroup group, NonNullList<ItemStack> items) {
		super.fillItemGroup(group, items);

		if (!isInGroup(group) || this != ModItems.BACKPACK) {
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
	public ActionResultType onItemUse(ItemUseContext context) {
		PlayerEntity player = context.getPlayer();
		if (player == null || !player.isSneaking()) {
			return ActionResultType.PASS;
		}

		BlockItemUseContext blockItemUseContext = new BlockItemUseContext(context);
		if (!blockItemUseContext.canPlace()) {
			return ActionResultType.FAIL;
		}
		World world = context.getWorld();
		BlockPos pos = context.getPos().offset(context.getFace());
		BlockState placementState = blockSupplier.get().getDefaultState().with(BackpackBlock.FACING, player.getHorizontalFacing().getOpposite());
		if (world.setBlockState(pos, placementState, 11)) {
			ItemStack backpack = context.getItem();
			WorldHelper.getTile(world, pos, BackpackTileEntity.class).ifPresent(te -> te.setBackpack(new BackpackWrapper(backpack.copy())));

			SoundType soundtype = placementState.getSoundType(world, pos, player);
			world.playSound(player, pos, soundtype.getPlaceSound(), SoundCategory.BLOCKS, (soundtype.getVolume() + 1.0F) / 2.0F, soundtype.getPitch() * 0.8F);
			if (!player.abilities.isCreativeMode) {
				backpack.shrink(1);
			}

			return ActionResultType.SUCCESS;
		}
		return super.onItemUse(context);
	}

	@Override
	public ActionResult<ItemStack> onItemRightClick(World world, PlayerEntity player, Hand hand) {
		ItemStack stack = player.getHeldItem(hand);

		if (!world.isRemote && player instanceof ServerPlayerEntity) {
			String handlerName = hand == Hand.MAIN_HAND ? PlayerInventoryProvider.MAIN_INVENTORY : PlayerInventoryProvider.OFFHAND_INVENTORY;
			int slot = hand == Hand.MAIN_HAND ? player.inventory.currentItem : 0;
			NetworkHooks.openGui((ServerPlayerEntity) player, new SimpleNamedContainerProvider((w, p, pl) -> new BackpackContainer(w, pl, handlerName, slot), stack.getDisplayName()),
					buf -> {
						buf.writeString(handlerName);
						buf.writeInt(slot);
					});
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

	@Nullable
	@Override
	public EquipmentSlotType getEquipmentSlot(ItemStack stack) {
		return EquipmentSlotType.CHEST;
	}
}