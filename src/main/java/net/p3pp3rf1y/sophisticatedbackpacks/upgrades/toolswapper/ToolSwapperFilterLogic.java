package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraftforge.common.ToolType;
import net.minecraftforge.common.extensions.IForgeItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.registry.tool.ToolRegistry;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.Comparator;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.function.Consumer;

public class ToolSwapperFilterLogic extends FilterLogicBase {
	private ItemStack weaponFilter;
	private final Map<ToolType, ItemStack> toolFilters = new TreeMap<>(Comparator.comparing(ToolType::getName));

	public ToolSwapperFilterLogic(ItemStack upgrade, Consumer<ItemStack> saveHandler) {
		super(upgrade, saveHandler, "");
		weaponFilter = NBTHelper.getCompound(upgrade, "weaponFilter").map(ItemStack::of).orElse(ItemStack.EMPTY);
		ToolRegistry.getToolTypes().forEach((name, tt) -> toolFilters.put(tt, ItemStack.EMPTY));
		loadToolFilters();
	}

	public ItemStack getWeaponFilter() {
		return weaponFilter;
	}

	public ItemStack getToolFilter(ToolType toolType) {
		return toolFilters.get(toolType);
	}

	public Set<ToolType> getToolFilterTypes() {
		return toolFilters.keySet();
	}

	private void loadToolFilters() {
		NBTHelper.getMap(upgrade, "toolFilters", key -> key, (key, nbt) -> ItemStack.of((CompoundNBT) nbt)).ifPresent(savedToolFilters ->
				savedToolFilters.forEach((key, stack) -> {
					Map<String, ToolType> toolTypes = ToolRegistry.getToolTypes();
					if (toolTypes.containsKey(key)) {
						toolFilters.put(toolTypes.get(key), stack);
					}
				}));
	}

	public boolean matchesWeaponFilter(ItemStack stack) {
		return !stack.isEmpty() && stack.getMaxStackSize() == 1 && isAllowList() == stackMatchesFilter(stack, weaponFilter);
	}

	public boolean matchesToolFilter(ItemStack stack, ToolType toolType) {
		return !stack.isEmpty() && stack.getMaxStackSize() == 1 && isAllowList() == stackMatchesFilter(stack, toolFilters.getOrDefault(toolType, ItemStack.EMPTY));
	}

	public void setWeaponFilter(ItemStack stack) {
		weaponFilter = stack;
		NBTHelper.setCompoundNBT(upgrade, "weaponFilter", stack.serializeNBT());
		save();
	}

	public void setToolFilter(ToolType toolType, ItemStack stack) {
		toolFilters.put(toolType, stack);
		NBTHelper.setMap(upgrade, "toolFilters", toolFilters, ToolType::getName, IForgeItemStack::serializeNBT);
		save();
	}
}