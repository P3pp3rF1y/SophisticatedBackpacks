package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import net.minecraft.inventory.container.PlayerContainer;
import net.minecraft.inventory.container.Slot;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.common.ToolType;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.registry.tool.ToolRegistry;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IServerUpdater;
import org.apache.commons.lang3.StringUtils;

import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Supplier;

public class ToolSwapperFilterContainer extends FilterLogicContainerBase<ToolSwapperFilterLogic, ToolFilterSlot> {
	private static final Set<String> AVAILABLE_TOOL_BACKGROUNDS = ImmutableSet.of("axe", "hammer", "pickaxe", "shovel", "hoe");
	public static final ResourceLocation EMPTY_WEAPON_SLOT_BACKGROUND = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "item/empty_weapon_slot");
	public static final Map<ToolType, ResourceLocation> EMPTY_TOOL_SLOT_BACKGROUNDS;

	static {
		ImmutableMap.Builder<ToolType, ResourceLocation> builder = new ImmutableMap.Builder<>();
		String template = "item/empty_%s_slot";
		for (ToolType toolType : ToolRegistry.getToolTypes().values()) {
			if (AVAILABLE_TOOL_BACKGROUNDS.contains(toolType.getName())) {
				builder.put(toolType, new ResourceLocation(SophisticatedBackpacks.MOD_ID, String.format(template, toolType.getName())));
			}
		}
		EMPTY_TOOL_SLOT_BACKGROUNDS = builder.build();
	}

	public ToolSwapperFilterContainer(IServerUpdater serverUpdater, Supplier<ToolSwapperFilterLogic> filterLogic, Consumer<Slot> addSlot) {
		super(serverUpdater, filterLogic);
		ToolFilterSlot weaponFilterSlot = new ToolFilterSlot(() -> filterLogic.get().getWeaponFilter(), stack -> filterLogic.get().setWeaponFilter(stack), s -> true);
		weaponFilterSlot.setBackground(PlayerContainer.BLOCK_ATLAS, EMPTY_WEAPON_SLOT_BACKGROUND);
		filterSlots.add(weaponFilterSlot);
		filterLogic.get().getToolFilterTypes().forEach(toolType ->
				{
					ToolFilterSlot toolFilterSlot = new ToolFilterSlot(() -> filterLogic.get().getToolFilter(toolType), stack -> filterLogic.get().setToolFilter(toolType, stack), s -> s.getToolTypes().contains(toolType));
					if (EMPTY_TOOL_SLOT_BACKGROUNDS.containsKey(toolType)) {
						toolFilterSlot.setBackground(PlayerContainer.BLOCK_ATLAS, EMPTY_TOOL_SLOT_BACKGROUNDS.get(toolType));
					}
					toolFilterSlot.setEmptyTooltip(StringUtils.capitalize(toolType.getName()));

					filterSlots.add(toolFilterSlot);
				}
		);
		filterSlots.forEach(addSlot);
	}
}
