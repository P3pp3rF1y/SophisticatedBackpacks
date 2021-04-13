package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper;

import com.google.common.collect.Multimap;
import com.google.common.util.concurrent.AtomicDouble;
import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.entity.ai.attributes.Attribute;
import net.minecraft.entity.ai.attributes.AttributeModifier;
import net.minecraft.entity.ai.attributes.Attributes;
import net.minecraft.entity.ai.attributes.ModifiableAttributeInstance;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.inventory.EquipmentSlotType;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Hand;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.common.ToolType;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IAttackEntityResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBlockClickResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

public class ToolSwapperUpgradeWrapper extends UpgradeWrapperBase<ToolSwapperUpgradeWrapper, ToolSwapperUpgradeItem>
		implements IBlockClickResponseUpgrade, IAttackEntityResponseUpgrade {
	protected ToolSwapperUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
	}

	@Override
	public boolean onBlockClick(PlayerEntity player, BlockPos pos) {
		if (player.isCreative() || player.isSpectator()) {
			return false;
		}

		ItemStack mainHandItem = player.getHeldItemMainhand();
		if (mainHandItem.getItem() instanceof BackpackItem || !(isTool(mainHandItem) || isSword(mainHandItem, player))) {
			return false;
		}

		BlockState state = player.world.getBlockState(pos);
		Block block = state.getBlock();

		if (anyToolTypeEffectiveOnBlock(state, block, mainHandItem)) {
			return true;
		}

		return tryToSwapTool(player, state, block, mainHandItem);
	}

	private boolean tryToSwapTool(PlayerEntity player, BlockState state, Block block, ItemStack mainHandItem) {
		AtomicBoolean swappedInEffectiveTool = new AtomicBoolean(false);
		AtomicBoolean noSpaceForItem = new AtomicBoolean(false);
		IItemHandlerModifiable backpackInventory = backpackWrapper.getInventoryHandler();
		InventoryHelper.iterate(backpackInventory, (slot, stack) -> {
			if (anyToolTypeEffectiveOnBlock(state, block, stack)
					&& (stack.getCount() == 1 || InventoryHelper.insertIntoInventory(mainHandItem, backpackInventory, true).isEmpty())) {
				player.setHeldItem(Hand.MAIN_HAND, backpackInventory.extractItem(slot, 1, false));
				InventoryHelper.insertIntoInventory(mainHandItem, backpackInventory, false);
				swappedInEffectiveTool.set(true);
			}
		}, () -> swappedInEffectiveTool.get() || noSpaceForItem.get());
		return swappedInEffectiveTool.get();
	}

	private boolean anyToolTypeEffectiveOnBlock(BlockState state, Block block, ItemStack mainHandItem) {
		for (ToolType type : mainHandItem.getToolTypes()) {
			if (block.isToolEffective(state, type)) {
				return true;
			}
		}
		return false;
	}

	@Override
	public boolean onAttackEntity(PlayerEntity player) {
		ItemStack mainHandItem = player.getHeldItemMainhand();
		if (mainHandItem.getItem() instanceof BackpackItem || !isTool(mainHandItem)) {
			return false;
		}
		if (isSword(mainHandItem, player)) {
			return true;
		}

		return tryToSwapInWeapon(player, mainHandItem);
	}

	private boolean isTool(ItemStack stack) {
		return !stack.getToolTypes().isEmpty();
	}

	private boolean isSword(ItemStack stack, PlayerEntity player) {
		ModifiableAttributeInstance attackDamage = player.getAttribute(Attributes.ATTACK_DAMAGE);
		if (!stack.isEmpty() && stack.getToolTypes().isEmpty()) {
			return attackDamage != null && attackDamage.getModifier(Item.ATTACK_DAMAGE_MODIFIER) != null;
		}
		return false;
	}

	private boolean tryToSwapInWeapon(PlayerEntity player, ItemStack mainHandItem) {
		AtomicReference<ItemStack> bestAxe = new AtomicReference<>(ItemStack.EMPTY);
		AtomicDouble bestAxeDamage = new AtomicDouble(0);
		AtomicReference<ItemStack> bestSword = new AtomicReference<>(ItemStack.EMPTY);
		AtomicDouble bestSwordDamage = new AtomicDouble(0);

		updateBestWeapons(bestAxe, bestAxeDamage, bestSword, bestSwordDamage, mainHandItem);

		IItemHandlerModifiable backpackInventory = backpackWrapper.getInventoryHandler();
		InventoryHelper.iterate(backpackInventory, (slot, stack) -> updateBestWeapons(bestAxe, bestAxeDamage, bestSword, bestSwordDamage, stack));

		if (!bestSword.get().isEmpty()) {
			return swapWeapon(player, mainHandItem, backpackInventory, bestSword.get());
		} else if (!bestAxe.get().isEmpty()) {
			return swapWeapon(player, mainHandItem, backpackInventory, bestAxe.get());
		}
		return false;
	}

	private void updateBestWeapons(AtomicReference<ItemStack> bestAxe, AtomicDouble bestAxeDamage, AtomicReference<ItemStack> bestSword, AtomicDouble bestSwordDamage, ItemStack stack) {
		if (stack.isEmpty() || stack.getMaxStackSize() > 1) {
			return;
		}

		ModifiableAttributeInstance attribute = new ModifiableAttributeInstance(Attributes.ATTACK_DAMAGE, a -> {});
		Multimap<Attribute, AttributeModifier> attributeModifiers = stack.getAttributeModifiers(EquipmentSlotType.MAINHAND);
		if (!attributeModifiers.containsKey(Attributes.ATTACK_DAMAGE)) {
			return;
		}
		attributeModifiers.get(Attributes.ATTACK_DAMAGE).forEach(m -> {
			attribute.removeModifier(m);
			attribute.applyNonPersistentModifier(m);
		});
		double damageValue = attribute.getValue();
		if (stack.getToolTypes().contains(ToolType.AXE)) {
			if (damageValue > bestAxeDamage.get()) {
				bestAxe.set(stack);
				bestAxeDamage.set(damageValue);
			}
		} else if (stack.getToolTypes().isEmpty() && damageValue > bestSwordDamage.get()) {
			bestSword.set(stack);
			bestSwordDamage.set(damageValue);
		}
	}

	private boolean swapWeapon(PlayerEntity player, ItemStack mainHandItem, IItemHandlerModifiable backpackInventory, ItemStack sword) {
		if (sword == mainHandItem) {
			return true;
		}

		InventoryHelper.extractFromInventory(sword, backpackInventory, false);
		if (InventoryHelper.insertIntoInventory(mainHandItem, backpackInventory, true).isEmpty()) {
			player.setHeldItem(Hand.MAIN_HAND, sword);
			InventoryHelper.insertIntoInventory(mainHandItem, backpackInventory, false);
			return true;
		} else {
			InventoryHelper.insertIntoInventory(sword, backpackInventory, false);
			return false;
		}
	}

}
